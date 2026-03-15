# 02_iv_calculation.R
# Computes BSM implied volatility for each option contract using
# numerical root-finding (uniroot). Uses OTM options only, which
# is the industry standard for building vol surfaces — OTM options
# tend to be more liquid and have tighter bid-ask spreads than ITM.
#
# Inputs  : data/options_data.rds  (from 01_fetch_data.R)
# Outputs : data/iv_surface.rds
#           data/iv_surface.csv
#           data/iv_summary.csv

library(dplyr)
library(readr)
library(tidyr)


# Prices a European call or put using the Black-Scholes-Merton formula.
# Returns the theoretical option price given spot, strike, time, rate,
# and vol. Used as the objective function inside the IV solver below.
bsm_price <- function(S, K, T, r, sigma, type = "call") {
  T     <- max(T, 1e-6)
  sigma <- max(sigma, 1e-6)
  d1    <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2    <- d1 - sigma * sqrt(T)
  if (type == "call") {
    S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else {
    K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  }
}


# Inverts the BSM formula to back out the implied volatility for a
# given market price. We bracket the root between 0.01% and 2000%
# vol — anything outside that range is almost certainly a data error.
# Returns NA if the price is below intrinsic value or if the solver
# fails to converge (e.g. deep ITM, stale quotes).
implied_vol <- function(price, S, K, T, r, type = "call") {
  T <- max(T, 1e-6)
  
  # An option price below intrinsic has no real IV solution
  intrinsic <- if (type == "call") max(S - K * exp(-r * T), 0)
  else                max(K * exp(-r * T) - S, 0)
  
  if (price <= intrinsic + 1e-6) return(NA_real_)
  
  tryCatch({
    result <- uniroot(
      f        = function(sigma) bsm_price(S, K, T, r, sigma, type) - price,
      interval = c(1e-4, 20),
      tol      = 1e-6,
      maxiter  = 1000
    )
    iv <- result$root
    if (iv < 0.01 || iv > 10) NA_real_ else iv
  }, error = function(e) NA_real_)
}


# Orchestrates the full IV calculation pipeline: loads raw options,
# applies OTM and quality filters, solves for IV on each contract,
# builds a per-expiry summary table, and optionally overlays the
# SVI fitted surface if svi_params.rds already exists on disk.
calculate_iv_surface <- function(r = 0.05) {
  
  options_data    <- readRDS("data/options_data.rds")
  df              <- options_data$options
  S0              <- options_data$spot
  expiry_date_map <- options_data$expiry_date_map
  
  cat("IV calculation starting\n")
  cat("Contracts loaded:", nrow(df), " | Spot: $", round(S0, 2), "\n\n")
  
  # Keep only OTM contracts — calls with K above 95% of spot and
  # puts with K below 105% of spot. Near-ATM contracts appear in
  # both sides, so we use a small overlap band to avoid gaps.
  df_otm <- df %>%
    filter(
      !(type == "call" & log_strike < -0.05),
      !(type == "put"  & log_strike >  0.05)
    )
  
  cat("After OTM filter:", nrow(df_otm), "contracts\n")
  
  # Quality filters: remove contracts that are too deep OTM,
  # too cheap, too wide bid-ask, or expiring in under 3 days.
  df_clean <- df_otm %>%
    filter(
      abs(log_strike) <= 0.35,
      mid_price       >= 0.05,
      Ask - Bid       <= mid_price * 0.50,
      T               >  3 / 365
    )
  
  cat("After quality filter:", nrow(df_clean), "contracts\n\n")
  cat("Solving for IV — this takes a few seconds...\n")
  
  # Vectorised IV solve across all contracts
  df_clean$iv <- mapply(
    FUN      = implied_vol,
    price    = df_clean$mid_price,
    S        = df_clean$spot,
    K        = df_clean$Strike,
    T        = df_clean$T,
    r        = r,
    type     = df_clean$type,
    SIMPLIFY = TRUE
  )
  
  # Drop contracts where IV is missing or clearly unreliable
  df_iv <- df_clean %>%
    filter(
      !is.na(iv),
      iv >= 0.02,   # below 2% is almost certainly a data error
      iv <= 2.50    # above 250% is illiquid/broken quote
    ) %>%
    mutate(total_var = iv^2 * T)
  
  cat("Valid IVs:", nrow(df_iv), "\n")
  cat("Removed (failed / extreme):", nrow(df) - nrow(df_iv), "\n\n")
  
  # Quick sanity check — confirm both calls and puts have coverage
  # across a reasonable moneyness range
  cat("Coverage by type:\n")
  df_iv %>%
    group_by(type) %>%
    summarise(
      n      = n(),
      min_ls = round(min(log_strike), 4),
      max_ls = round(max(log_strike), 4),
      .groups = "drop"
    ) %>%
    print()
  cat("\n")
  
  # Per-expiry summary table used in the Shiny dashboard Summary tab.
  # ATM IV uses a ±5% moneyness band to capture both calls and puts.
  # Put skew = OTM put IV minus ATM IV (positive = left skew, normal
  # for SPY where crash protection is persistently bid up).
  summary_tbl <- df_iv %>%
    group_by(days_exp) %>%
    summarise(
      n       = n(),
      n_calls = sum(type == "call", na.rm = TRUE),
      n_puts  = sum(type == "put",  na.rm = TRUE),
      
      atm_iv  = mean(iv[abs(log_strike) < 0.05], na.rm = TRUE),
      min_iv  = min(iv,  na.rm = TRUE),
      max_iv  = max(iv,  na.rm = TRUE),
      
      put_skew = {
        atm_  <- mean(iv[abs(log_strike) < 0.05], na.rm = TRUE)
        otmp_ <- mean(
          iv[type == "put" & log_strike <= -0.05 & log_strike >= -0.35],
          na.rm = TRUE
        )
        otmp_ - atm_
      },
      
      call_skew = {
        atm_  <- mean(iv[abs(log_strike) < 0.05], na.rm = TRUE)
        otmc_ <- mean(
          iv[type == "call" & log_strike >= 0.05],
          na.rm = TRUE
        )
        otmc_ - atm_
      },
      
      .groups = "drop"
    ) %>%
    mutate(
      atm_iv    = round(atm_iv,    4),
      min_iv    = round(min_iv,    4),
      max_iv    = round(max_iv,    4),
      put_skew  = round(put_skew,  4),
      call_skew = round(call_skew, 4)
    )
  
  # Attach actual calendar dates to the summary table so downstream
  # code can display "19/03/2026" instead of just "4d"
  summary_tbl <- summary_tbl %>%
    left_join(expiry_date_map, by = "days_exp")
  
  cat("IV summary by expiry:\n")
  print(summary_tbl)
  
  nan_count <- sum(is.nan(summary_tbl$put_skew))
  if (nan_count > 0) {
    cat("\nWarning: put_skew is NaN for", nan_count,
        "expiries — likely insufficient OTM put coverage\n")
  } else {
    cat("\nput_skew looks clean — no NaN values\n")
  }
  
  # If SVI params are already on disk (i.e. script 03 has run before),
  # rebuild the smooth fitted surface and attach it to the output.
  # This lets script 02 be re-run independently without losing the fit.
  fitted_df <- NULL
  if (file.exists("data/svi_params.rds")) {
    svi_params  <- readRDS("data/svi_params.rds")
    k_grid      <- seq(-0.35, 0.35, length.out = 200)
    fitted_rows <- list()
    
    for (exp_key in names(svi_params)) {
      fit   <- svi_params[[exp_key]]
      p     <- fit$params
      T_    <- fit$T
      days_ <- fit$days
      w_vec <- p[1] + p[2] * (
        p[3] * (k_grid - p[4]) + sqrt((k_grid - p[4])^2 + p[5]^2)
      )
      iv_fit <- sqrt(pmax(w_vec / T_, 0))
      fitted_rows[[exp_key]] <- data.frame(
        log_strike = k_grid,
        iv_fit     = iv_fit,
        days_exp   = days_,
        T          = T_
      )
    }
    
    fitted_df <- bind_rows(fitted_rows)
    cat("SVI fitted surface loaded from disk\n")
  }
  
  # Bundle everything the downstream scripts need into one object
  out <- list(
    iv_df           = df_iv,
    fitted_surface  = fitted_df,
    summary         = summary_tbl,
    spot            = S0,
    r               = r,
    surface         = df_iv,
    fetch_time      = Sys.time(),
    expiry_date_map = expiry_date_map
  )
  
  saveRDS(out, "data/iv_surface.rds")
  write_csv(df_iv,       "data/iv_surface.csv")
  write_csv(summary_tbl, "data/iv_summary.csv")
  
  cat("\nSaved: data/iv_surface.rds, iv_surface.csv, iv_summary.csv\n")
  
  return(df_iv)
}

result <- calculate_iv_surface(r = 0.05)
