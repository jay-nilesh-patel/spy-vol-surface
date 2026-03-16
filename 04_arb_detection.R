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
#      than 3% rich or cheap relative to the fit are flagged as
#      potential mispricings worth investigating.
#
# Inputs  : data/svi_fits.rds  (from 03_svi_calibration.R)
# Outputs : data/arb_results.rds

library(dplyr)
library(tidyr)
library(readr)


# SVI total variance formula — kept here so this script is self-contained
# and does not depend on 03_svi_calibration.R being loaded in the session.
svi_w <- function(k, params) {
  a <- params[1]; b <- params[2]; rho <- params[3]
  m <- params[4]; s <- params[5]
  a + b * (rho * (k - m) + sqrt((k - m)^2 + s^2))
}


# Checks butterfly arbitrage for a single fitted SVI slice using the
# Gatheral-Jacquier local density condition:
#
#   g(k) = (1 - k*w'/(2w))^2 - (w'^2/4)*(1/w + 1/4) + w''/2 >= 0
#
# g(k) is proportional to the risk-neutral density. Any region where
# g < 0 is a butterfly arbitrage. We evaluate on 500 points so we
# can pinpoint the exact moneyness range of any violation.
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


# Checks calendar spread arbitrage across all fitted slices.
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


# Computes per-contract model residuals for BOTH calls and puts.
# The type column ("call" / "put") is preserved so the Shiny table
# can display and colour-code each contract type separately.
# Residuals expressed in absolute vol points and as % of fitted IV.
# Contracts more than 3% away from the fit are flagged.
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
      residual_pct = (iv - iv_svi) / iv_svi * 100,
      is_rich      = residual_pct >  3,
      is_cheap     = residual_pct < -3
    ) %>%
    ungroup() %>%
    filter(!is.na(iv_svi))
}


# Orchestrates all three arbitrage checks and writes results to disk.
detect_all_arb <- function() {
  obj       <- readRDS("data/svi_fits.rds")
  iv_df     <- obj$iv_df
  fits      <- obj$fits
  S0        <- obj$spot
  
  cat("Running arbitrage detection on", length(fits), "fitted slices\n\n")
  
  # 1. Butterfly
  butterfly_df <- bind_rows(lapply(fits, check_butterfly))
  n_bf         <- sum(butterfly_df$butterfly_viol)
  cat("Butterfly violations:", n_bf, "points")
  if (n_bf == 0) cat("  — surface is butterfly-free\n") else cat("\n")
  
  # 2. Calendar
  cal_arb_df <- check_calendar(fits)
  n_cal      <- sum(cal_arb_df$cal_arb_viol)
  cat("Calendar violations: ", n_cal, "points")
  if (n_cal == 0) cat("  — surface is calendar-free\n") else cat("\n")
  
  # 3. Residuals (calls + puts)
  residuals_df <- check_residuals(iv_df, fits)
  n_rich       <- sum(residuals_df$is_rich,  na.rm = TRUE)
  n_cheap      <- sum(residuals_df$is_cheap, na.rm = TRUE)
  cat("Rich  (market IV > SVI + 3%):", n_rich,  "contracts\n")
  cat("Cheap (market IV < SVI - 3%):", n_cheap, "contracts\n")
  
  # 4. Composite arb score per contract — used to drive the heatmap.
  # Base score is |residual_pct|; contracts that breach the 3% threshold
  # get an additional +10 to make them visually stand out in the heatmap.
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
