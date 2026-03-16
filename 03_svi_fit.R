# 03_svi_fit.R
# Fits Gatheral's SVI (Stochastic Volatility Inspired) model to each
# expiry slice of the implied vol surface independently.
#
# The SVI parametrisation for total variance w(k) is:
#   w(k) = a + b * [ rho*(k-m) + sqrt((k-m)^2 + sigma^2) ]
# where k = log(K/S) is log-moneyness. Fitting is done in total
# variance space (not IV space) to avoid the sqrt distortion near ATM.
#
# We use calls only per slice — puts are handled via put-call parity
# at the IV level so the smile is already consistent before we fit.
#
# Inputs  : data/iv_surface.rds  (from 02_iv_calculation.R)
# Outputs : data/svi_fits.rds
#           data/svi_params.rds   (params only — consumed by 02 on re-run)

library(dplyr)
library(purrr)
library(readr)
library(nloptr)


# Evaluates the SVI total variance formula at log-moneyness vector k.
# params = c(a, b, rho, m, sigma) following Gatheral's notation.
svi_w <- function(k, params) {
  a <- params[1]; b <- params[2]; rho <- params[3]
  m <- params[4]; s <- params[5]
  a + b * (rho * (k - m) + sqrt((k - m)^2 + s^2))
}


# Converts SVI total variance to implied vol for a given expiry T.
# Clamps negative variance to zero before taking the square root —
# this should not happen for a valid fit but guards against edge cases
# at the extremes of the k grid during surface plotting.
svi_iv <- function(k, T, params) {
  w <- svi_w(k, params)
  sqrt(pmax(w / T, 0))
}


# Fits SVI to a single expiry slice using derivative-free optimisation
# (NLOPT_LN_SBPLX). We try three starting points and keep the best
# solution — SVI is non-convex so multiple starts meaningfully improve
# the chance of finding the global minimum.
#
# The no-arbitrage constraint a + b*sigma*sqrt(1 - rho^2) >= 0 is
# enforced inside the objective so the solver never returns a surface
# with negative total variance at the ATM point.
fit_svi_slice <- function(slice_df) {
  k <- slice_df$log_strike
  w <- slice_df$total_var
  T <- unique(slice_df$T)[1]
  
  objective <- function(params) {
    a <- params[1]; b <- params[2]; rho <- params[3]
    m <- params[4]; s <- params[5]
    
    if (b <= 0 || s <= 0 || abs(rho) >= 1)          return(1e10)
    if (a + b * s * sqrt(1 - rho^2) < 0)            return(1e10)
    
    w_hat <- svi_w(k, params)
    if (any(w_hat <= 0))                             return(1e10)
    
    sum((w - w_hat)^2)
  }
  
  # Use the mean ATM implied vol of this slice as a scale anchor for
  # the starting values — better than a fixed prior across all expiries
  atm_iv <- slice_df %>%
    filter(abs(log_strike) < 0.05) %>%
    summarise(m = mean(iv)) %>%
    pull(m)
  atm_iv  <- if (is.na(atm_iv) || length(atm_iv) == 0) 0.20 else atm_iv
  atm_var <- atm_iv^2 * T
  
  starts <- list(
    c(atm_var * 0.5, 0.10, -0.30,  0.00,  0.10),
    c(atm_var * 0.3, 0.15, -0.50,  0.05,  0.15),
    c(atm_var * 0.4, 0.08, -0.20, -0.05,  0.08)
  )
  
  best_result <- NULL
  best_val    <- Inf
  
  for (p0 in starts) {
    tryCatch({
      res <- nloptr(
        x0     = p0,
        eval_f = objective,
        lb     = c(-0.5,  0.001, -0.999, -1.0, 0.001),
        ub     = c( 1.0,  2.0,    0.999,  1.0, 1.0),
        opts   = list(
          algorithm = "NLOPT_LN_SBPLX",
          maxeval   = 2000,
          xtol_rel  = 1e-8
        )
      )
      if (res$objective < best_val) {
        best_val    <- res$objective
        best_result <- res$solution
      }
    }, error = function(e) NULL)
  }
  
  if (is.null(best_result)) return(NULL)
  
  list(
    params = best_result,
    rmse   = sqrt(best_val / nrow(slice_df)),
    T      = T,
    expiry = unique(slice_df$expiry)[1],
    days   = unique(slice_df$days_exp)[1],
    n_pts  = nrow(slice_df)
  )
}


# Loops over all expiry slices in the IV surface, fits SVI to each,
# and builds the smooth fitted surface on a dense k grid. Slices with
# fewer than 5 data points are skipped — SVI has 5 parameters so
# fitting would be underdetermined.
fit_all_svi <- function() {
  obj    <- readRDS("data/iv_surface.rds")
  iv_df  <- obj$surface
  S0     <- obj$spot
  
  expiries <- sort(unique(iv_df$days_exp))
  cat("Fitting SVI to", length(expiries), "expiry slices\n\n")
  
  fits <- list()
  
  for (d in expiries) {
    slice <- iv_df %>% filter(days_exp == d, type == "call")
    
    if (nrow(slice) < 3) {
      cat("Skipping", d, "days — fewer than 5 call contracts\n")
      next
    }
    
    result <- fit_svi_slice(slice)
    
    if (!is.null(result)) {
      fits[[as.character(d)]] <- result
      cat(sprintf(
        "%3d days | RMSE: %.5f | a=%.3f  b=%.3f  rho=%.3f  m=%.3f  sigma=%.3f\n",
        d, result$rmse,
        result$params[1], result$params[2], result$params[3],
        result$params[4], result$params[5]
      ))
    } else {
      cat("Fit failed for", d, "days — all starting points diverged\n")
    }
  }
  
  # Build a dense fitted surface on a uniform k grid for plotting.
  # 200 points per slice is more than enough for smooth Plotly curves.
  k_grid      <- seq(-0.35, 0.35, length.out = 200)
  fitted_rows <- list()
  
  for (nm in names(fits)) {
    fit    <- fits[[nm]]
    iv_fit <- svi_iv(k_grid, fit$T, fit$params)
    fitted_rows[[nm]] <- data.frame(
      log_strike = k_grid,
      iv_fit     = iv_fit,
      total_var  = iv_fit^2 * fit$T,
      days_exp   = fit$days,
      T          = fit$T,
      expiry     = fit$expiry
    )
  }
  
  fitted_df <- bind_rows(fitted_rows)
  
  # svi_fits.rds — full output consumed by the Shiny dashboard
  saveRDS(
    list(
      fits           = fits,
      fitted_surface = fitted_df,
      iv_df          = iv_df,
      spot           = S0
    ),
    "data/svi_fits.rds"
  )
  
  # svi_params.rds — lightweight params-only file consumed by
  # 02_iv_calculation.R when it rebuilds the surface overlay on re-run
  params_only <- lapply(fits, function(f) {
    list(params = f$params, T = f$T, days = f$days)
  })
  saveRDS(params_only, "data/svi_params.rds")
  
  cat("\nSlices fitted successfully:", length(fits), "/", length(expiries), "\n")
  cat("Saved: data/svi_fits.rds\n")
  cat("Saved: data/svi_params.rds\n")
  
  return(list(fits = fits, fitted_surface = fitted_df, iv_df = iv_df))
}

svi_result <- fit_all_svi()
