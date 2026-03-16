# 04_arb_detection.R
# Detects three classes of arbitrage in the fitted SVI surface:
#
#   1. Butterfly arbitrage  — checks that the risk-neutral density
#      implied by the SVI fit is non-negative everywhere. A negative
#      density means you can construct a butterfly spread that pays
#      off with probability zero but has a negative cost — free money.
#
#   2. Calendar spread arbitrage — total variance w(k, T) must be
#      non-decreasing in T at every fixed moneyness k. If a shorter
#      expiry has higher total variance than a longer one, you can
#      sell the short and buy the long for a riskless profit.
#
#   3. Model residuals — market IV vs SVI fitted IV. Contracts more
#      than 3 vol points rich or cheap relative to the fit are flagged
#      as potential mispricings worth investigating.
#
# Inputs  : data/svi_fits.rds  (from 03_svi_calibration.R)
# Outputs : data/arb_results.rds

library(dplyr)
library(tidyr)
library(readr)


svi_w <- function(k, params) {
  a <- params[1]; b <- params[2]; rho <- params[3]
  m <- params[4]; s <- params[5]
  a + b * (rho * (k - m) + sqrt((k - m)^2 + s^2))
}


check_butterfly <- function(fit) {
  k   <- seq(-0.35, 0.35, length.out = 500)
  w   <- sapply(k, function(ki) svi_w(ki, fit$params))
  dk  <- k[2] - k[1]
  
  dw   <- diff(w) / dk
  d2w  <- diff(diff(w)) / dk^2
  
  k_mid <- k[2:(length(k) - 1)]
  w_mid <- w[2:(length(w) - 1)]
  dw_m  <- (dw[1:(length(dw) - 1)] + dw[2:length(dw)]) / 2
  
  g <- (1 - k_mid * dw_m / (2 * w_mid))^2 -
    (dw_m^2 / 4) * (1 / w_mid + 1 / 4) +
    d2w / 2
  
  data.frame(
    log_strike     = k_mid,
    density        = g,
    butterfly_viol = g < 0,
    days_exp       = fit$days,
    T              = fit$T,
    expiry         = fit$expiry
  )
}


check_calendar <- function(fits) {
  k_check    <- seq(-0.2, 0.2, by = 0.02)
  exp_sorted <- names(fits)[order(sapply(fits, function(f) f$T))]
  
  rows <- list()
  
  for (ki in k_check) {
    prev_w   <- -Inf
    prev_day <- NA
    
    for (nm in exp_sorted) {
      fit  <- fits[[nm]]
      w_ki <- svi_w(ki, fit$params)
      is_viol <- w_ki < prev_w
      
      rows[[length(rows) + 1]] <- data.frame(
        log_strike   = ki,
        days_exp     = fit$days,
        T            = fit$T,
        total_var    = w_ki,
        cal_arb_viol = is_viol,
        prev_days    = prev_day
      )
      
      if (!is_viol) {
        prev_w   <- w_ki
        prev_day <- fit$days
      }
    }
  }
  
  bind_rows(rows)
}


# residual_pct = (iv - iv_svi) * 100  →  vol points difference (e.g. +2.3 pp)
# This is more interpretable than a ratio — a 3 vol point threshold means
# the market is pricing 3 percentage points away from the SVI fit.
check_residuals <- function(iv_df, fits) {
  iv_df %>%
    filter(type == "call") %>%
    rowwise() %>%
    mutate(
      fit_key      = as.character(days_exp),
      iv_svi       = {
        fit <- fits[[fit_key]]
        if (is.null(fit)) NA_real_
        else sqrt(max(svi_w(log_strike, fit$params) / T, 0))
      },
      residual     = iv - iv_svi,
      residual_pct = (iv - iv_svi) * 100,   # vol points, not relative ratio
      is_rich      = residual_pct >  3,      # market 3 vpts above model
      is_cheap     = residual_pct < -3       # market 3 vpts below model
    ) %>%
    ungroup() %>%
    filter(!is.na(iv_svi))
}


detect_all_arb <- function() {
  obj       <- readRDS("data/svi_fits.rds")
  iv_df     <- obj$iv_df
  fits      <- obj$fits
  S0        <- obj$spot
  
  cat("Running arbitrage detection on", length(fits), "fitted slices\n\n")
  
  butterfly_df <- bind_rows(lapply(fits, check_butterfly))
  n_bf         <- sum(butterfly_df$butterfly_viol)
  cat("Butterfly violations:", n_bf, "points")
  if (n_bf == 0) cat("  — surface is butterfly-free\n") else cat("\n")
  
  cal_arb_df <- check_calendar(fits)
  n_cal      <- sum(cal_arb_df$cal_arb_viol)
  cat("Calendar violations: ", n_cal, "points")
  if (n_cal == 0) cat("  — surface is calendar-free\n") else cat("\n")
  
  residuals_df <- check_residuals(iv_df, fits)
  n_rich       <- sum(residuals_df$is_rich,  na.rm = TRUE)
  n_cheap      <- sum(residuals_df$is_cheap, na.rm = TRUE)
  cat("Rich  (market IV > SVI + 3 vpts):", n_rich,  "contracts\n")
  cat("Cheap (market IV < SVI - 3 vpts):", n_cheap, "contracts\n")
  
  arb_score_df <- residuals_df %>%
    mutate(
      arb_score = abs(residual_pct) + ifelse(is_rich | is_cheap, 10, 0)
    )
  
  out <- list(
    butterfly  = butterfly_df,
    calendar   = cal_arb_df,
    residuals  = residuals_df,
    arb_score  = arb_score_df,
    fits       = fits,
    spot       = S0
  )
  
  saveRDS(out, "data/arb_results.rds")
  cat("\nSaved: data/arb_results.rds\n")
  
  return(out)
}

arb <- detect_all_arb()
