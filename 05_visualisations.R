# 05_visualisations.R
# Generates all six visualisations for the SPY vol surface dashboard.
# Can be sourced standalone or loaded by the Shiny app (in which case
# the standalone run block at the bottom is skipped via shiny_running).
#
# Outputs written to ../plots/:
#   01_iv_surface_3d.html      — interactive 3D SVI surface
#   02_smile_plots.png         — per-expiry smile + SVI fit overlay
#   03_arb_heatmap.png         — arb score heatmap across (k, T)
#   04_butterfly_violations.png — risk-neutral density g(k) per slice
#   05_calendar_arb.png        — total variance term structure
#   06_residuals_3d.html       — interactive 3D residuals surface
#
# Inputs  : ../data/arb_results.rds  (from 04_arb_detection.R)
#           ../data/svi_fits.rds     (from 03_svi_calibration.R)

library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(scales)
library(htmlwidgets)


# Colour palette — all plots share these tokens so a single change
# here propagates everywhere. Dark background keeps the vol surface
# colours vivid and matches the Shiny dashboard panel colours.
BG     <- "#000000"
PANEL  <- "#04060F"
GRID   <- "#0E1220"
BORDER <- "#1C2033"
TEXT   <- "#E4ECFF"
MUTED  <- "#6B7491"
ACCENT <- "#4CA3FF"
GOLD   <- "#F0B35A"
GREEN  <- "#2ECC9A"
RED    <- "#FF4D6D"
PURP   <- "#A78BFA"
ORANGE <- "#FF8C42"


# Base ggplot2 theme applied to all static plots. Inherits from
# theme_minimal and overrides every panel/text element so no white
# leaks through when the plot is embedded in the dark Shiny UI.
theme_vsl <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = BG,    color = NA),
      panel.background  = element_rect(fill = PANEL, color = NA),
      panel.grid.major  = element_line(color = GRID,  linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      text              = element_text(color = TEXT,  family = "sans"),
      axis.text         = element_text(color = MUTED, size = base_size - 3),
      axis.title        = element_text(color = ACCENT, size = base_size - 1,
                                       face = "bold"),
      plot.title        = element_text(color = TEXT,  size = base_size + 5,
                                       face = "bold", margin = margin(b = 6)),
      plot.subtitle     = element_text(color = MUTED, size = base_size - 2,
                                       margin = margin(b = 14)),
      plot.caption      = element_text(color = "#3A405A", size = base_size - 5,
                                       hjust = 1),
      legend.background = element_rect(fill = PANEL, color = BORDER),
      legend.text       = element_text(color = TEXT,  size = base_size - 2),
      legend.title      = element_text(color = ACCENT, size = base_size - 1,
                                       face = "bold"),
      legend.key        = element_rect(fill = PANEL,  color = NA),
      legend.key.size   = unit(1.2, "lines"),
      strip.background  = element_rect(fill = "#080B16", color = BORDER),
      strip.text        = element_text(color = ACCENT, face = "bold",
                                       size = base_size),
      plot.margin       = margin(20, 24, 16, 20)
    )
}


# SVI total variance formula — kept here so this script is fully
# self-contained and can be sourced without 03_svi_calibration.R.
svi_w <- function(k, params) {
  a <- params[1]; b <- params[2]; rho <- params[3]
  m <- params[4]; s <- params[5]
  a + b * (rho * (k - m) + sqrt((k - m)^2 + s^2))
}


# Reads both RDS files into the calling environment. Uses <<- so the
# objects are visible to all plot functions without passing them as
# arguments — keeps the function signatures clean for Shiny usage.
load_viz_data <- function() {
  arb_path <- "../data/arb_results.rds"
  svi_path <- "../data/svi_fits.rds"
  if (!file.exists(arb_path)) stop("arb_results.rds not found — run 04_arb_detection.R first")
  if (!file.exists(svi_path)) stop("svi_fits.rds not found — run 03_svi_calibration.R first")
  
  arb_obj      <<- readRDS(arb_path)
  svi_obj      <<- readRDS(svi_path)
  butterfly_df <<- arb_obj$butterfly
  cal_df       <<- arb_obj$calendar
  resid_df     <<- arb_obj$residuals
  arb_score_df <<- arb_obj$arb_score
  fits         <<- arb_obj$fits
  S0           <<- arb_obj$spot
  iv_df        <<- svi_obj$iv_df
  fitted_df    <<- svi_obj$fitted_surface
  
  cat("Data loaded | Spot: $", round(S0, 2),
      "| Expiries fitted:", length(fits), "\n")
}


# Wraps ggsave with a tryCatch so a single failed plot does not abort
# the rest of the run. ../plots/ is created if it does not exist.
safe_ggsave <- function(filename, plot, width, height, dpi = 180) {
  dir.create("../plots", showWarnings = FALSE, recursive = TRUE)
  tryCatch(
    ggsave(filename, plot, width = width, height = height, dpi = dpi, bg = BG),
    error = function(e) cat("Could not save", filename, ":", e$message, "\n")
  )
}

safe_savewidget <- function(fig, filename) {
  dir.create("../plots", showWarnings = FALSE, recursive = TRUE)
  tryCatch(
    saveWidget(fig, filename, selfcontained = TRUE),
    error = function(e) cat("Could not save", filename, ":", e$message, "\n")
  )
}


load_viz_data()


# Interactive 3D SVI surface. The IV matrix is evaluated on a 120x8
# grid; 120 k-points gives smooth contours without slowing down the
# browser. contours are disabled because Plotly's default projections
# cast a white shadow onto the scene floor.
plot_3d_surface <- function() {
  k_grid     <- seq(-0.30, 0.30, length.out = 120)
  exp_sorted <- names(fits)[order(sapply(fits, function(f) f$T))]
  days_vec   <- sapply(fits[exp_sorted], function(f) f$days)
  
  iv_matrix <- matrix(NA, nrow = length(days_vec), ncol = length(k_grid))
  for (i in seq_along(exp_sorted)) {
    fit <- fits[[exp_sorted[i]]]
    for (j in seq_along(k_grid)) {
      w <- svi_w(k_grid[j], fit$params)
      iv_matrix[i, j] <- sqrt(max(w / fit$T, 0))
    }
  }
  
  fig <- plot_ly() %>%
    add_surface(
      x          = k_grid,
      y          = days_vec,
      z          = iv_matrix,
      colorscale = list(
        c(0.00, "rgb(5,5,20)"),
        c(0.15, "rgb(20,30,100)"),
        c(0.35, "rgb(10,120,200)"),
        c(0.55, "rgb(0,210,160)"),
        c(0.75, "rgb(240,170,30)"),
        c(1.00, "rgb(255,60,80)")
      ),
      contours = list(
        x = list(show = FALSE),
        y = list(show = FALSE),
        z = list(show = FALSE)
      ),
      lighting = list(
        ambient   = 0.70,
        diffuse   = 0.85,
        roughness = 0.30,
        specular  = 0.60,
        fresnel   = 0.30
      ),
      opacity = 0.97
    ) %>%
    layout(
      title = list(
        text = paste0("SPY IMPLIED VOLATILITY SURFACE  ·  SVI  ·  SPOT $",
                      round(S0, 2)),
        font = list(color = TEXT, size = 18,
                    family = "system-ui, sans-serif"),
        x = 0.04
      ),
      scene = list(
        xaxis = list(
          title           = list(text = "LOG-MONEYNESS k",
                                 font = list(color = ACCENT, size = 12)),
          backgroundcolor = "#000000",
          gridcolor       = "#1A1F35",
          zerolinecolor   = "#2A3050",
          tickfont        = list(color = MUTED, size = 11),
          tickformat      = ".2f"
        ),
        yaxis = list(
          title           = list(text = "DAYS TO EXPIRY",
                                 font = list(color = ACCENT, size = 12)),
          backgroundcolor = "#000000",
          gridcolor       = "#1A1F35",
          zerolinecolor   = "#2A3050",
          tickfont        = list(color = MUTED, size = 11)
        ),
        zaxis = list(
          title           = list(text = "IMPLIED VOL",
                                 font = list(color = ACCENT, size = 12)),
          backgroundcolor = "#000000",
          gridcolor       = "#1A1F35",
          zerolinecolor   = "#2A3050",
          tickfont        = list(color = MUTED, size = 11),
          tickformat      = ".0%"
        ),
        bgcolor     = "#000000",
        camera      = list(eye = list(x = 1.7, y = -1.7, z = 1.1)),
        aspectmode  = "manual",
        aspectratio = list(x = 1.2, y = 1.2, z = 0.8)
      ),
      paper_bgcolor = BG,
      plot_bgcolor  = BG,
      font   = list(color = MUTED, size = 11,
                    family = "system-ui, sans-serif"),
      margin = list(l = 0, r = 0, t = 60, b = 0)
    )
  
  safe_savewidget(fig, "../plots/01_iv_surface_3d.html")
  cat("01_iv_surface_3d.html saved\n")
  return(fig)
}


# Per-expiry smile plots: SVI fit as a thick amber line, market IV as
# dots coloured by residual_pct. Dot size also encodes |residual_pct|
# so outliers are immediately visible even in greyscale print.
plot_smiles <- function() {
  p <- ggplot() +
    geom_line(
      data      = fitted_df,
      aes(x = log_strike, y = iv_fit),
      color     = GOLD,
      linewidth = 1.6,
      alpha     = 0.95
    ) +
    geom_point(
      data  = resid_df,
      aes(x = log_strike, y = iv,
          color = residual_pct,
          size  = abs(residual_pct)),
      alpha = 0.85
    ) +
    scale_color_gradientn(
      colors = c(GREEN, "#1A3040", TEXT, "#3A1525", RED),
      values = scales::rescale(c(
        min(resid_df$residual_pct, na.rm = TRUE),
        -2, 0, 2,
        max(resid_df$residual_pct, na.rm = TRUE)
      )),
      name  = "RESIDUAL %",
      guide = guide_colorbar(
        barwidth       = 16, barheight = 0.8,
        title.position = "top",
        title.theme    = element_text(color = ACCENT, size = 11,
                                      face = "bold", family = "sans")
      )
    ) +
    scale_size_continuous(range = c(1.5, 6), guide = "none") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~days_exp,
               labeller = label_bquote(.(days_exp) ~ "days"),
               ncol = 3, scales = "free_y") +
    labs(
      title    = "VOLATILITY SMILE — MARKET IV vs SVI FIT",
      subtitle = "Amber = SVI fit  ·  dots = market IV  ·  green = cheap  ·  red = rich",
      x        = "Log-Moneyness  k = log(K / S)",
      y        = "Implied Volatility",
      caption  = "SVI: Gatheral (2004)  ·  Data: Yahoo Finance"
    ) +
    theme_vsl(base_size = 14) +
    theme(legend.position = "bottom")
  
  safe_ggsave("../plots/02_smile_plots.png", p, width = 20, height = 14)
  cat("02_smile_plots.png saved\n")
  return(p)
}


# Heatmap of composite arb scores across (log-moneyness, expiry).
# Strikes are bucketed to 2% bins so the tiles are wide enough to
# read. Labels are printed only above 3% to avoid clutter.
plot_arb_heatmap <- function() {
  heatmap_df <- arb_score_df %>%
    mutate(strike_bucket = round(log_strike / 0.02) * 0.02) %>%
    group_by(strike_bucket, days_exp) %>%
    summarise(arb_score = mean(arb_score, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(heatmap_df,
              aes(x = strike_bucket, y = factor(days_exp), fill = arb_score)) +
    geom_tile(color = BG, linewidth = 0.6) +
    geom_text(
      aes(label = ifelse(arb_score > 3, sprintf("%.0f%%", arb_score), "")),
      color = "#FFFFFF", size = 4.5, fontface = "bold"
    ) +
    scale_fill_gradientn(
      colors = c(PANEL, "#0A1830", "#0D3060", "#1060A0",
                 ACCENT, GOLD, ORANGE, RED),
      values = scales::rescale(c(0, 1, 3, 5, 8, 12, 20, 35)),
      name   = "ARB SCORE",
      guide  = guide_colorbar(
        barwidth       = 18, barheight = 0.8,
        title.position = "top",
        title.theme    = element_text(color = ACCENT, size = 11,
                                      face = "bold", family = "sans")
      )
    ) +
    scale_x_continuous(
      labels = function(x) sprintf("%+.0f%%", x * 100),
      breaks = seq(-0.3, 0.3, by = 0.04)
    ) +
    labs(
      title    = "VOLATILITY ARBITRAGE HEATMAP",
      subtitle = "Score = |market IV − SVI fit| %  ·  bright = high mispricing  ·  labels shown above 3%",
      x        = "Log-Moneyness  k = log(K / S)",
      y        = "Days to Expiry",
      caption  = "Threshold: score > 3% flagged as potential arb opportunity"
    ) +
    theme_vsl(base_size = 14) +
    theme(legend.position = "bottom")
  
  safe_ggsave("../plots/03_arb_heatmap.png", p, width = 18, height = 11)
  cat("03_arb_heatmap.png saved\n")
  return(p)
}


# Risk-neutral density g(k) per expiry slice. A red ribbon fills any
# region where g < 0. Per-panel free_y scales matter here because the
# magnitude of g varies considerably across expiries — fixed scales
# would flatten the short-dated panels into a flat line.
plot_butterfly <- function() {
  bf_clean <- butterfly_df %>%
    group_by(days_exp) %>%
    mutate(
      q99     = quantile(density, 0.99, na.rm = TRUE),
      q01     = quantile(density, 0.01, na.rm = TRUE),
      density = pmin(pmax(density, q01), q99)
    ) %>%
    ungroup()
  
  p <- ggplot(bf_clean,
              aes(x     = log_strike,
                  y     = density,
                  color = butterfly_viol,
                  group = factor(days_exp))) +
    geom_hline(yintercept = 0, color = RED,
               linetype = "dashed", linewidth = 0.8, alpha = 0.7) +
    geom_ribbon(
      data  = bf_clean %>% filter(butterfly_viol),
      aes(ymin = density, ymax = 0, group = factor(days_exp)),
      fill  = RED, alpha = 0.18, color = NA
    ) +
    geom_line(linewidth = 1.4, alpha = 0.95) +
    scale_color_manual(
      values = c("FALSE" = GREEN, "TRUE" = RED),
      labels = c("FALSE" = "Valid density", "TRUE" = "Butterfly violation"),
      name   = ""
    ) +
    facet_wrap(~days_exp,
               labeller = label_bquote(.(days_exp) ~ "days"),
               ncol = 3, scales = "free_y") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title    = "BUTTERFLY ARBITRAGE — RISK-NEUTRAL DENSITY g(k)",
      subtitle = "g(k) < 0 = negative probability = butterfly violation  ·  each panel = independent scale",
      x        = "Log-Moneyness  k = log(K / S)",
      y        = "Risk-Neutral Density  g(k)",
      caption  = "Breeden-Litzenberger (1978):  g(k) = e^(rT) · d²C/dK²"
    ) +
    theme_vsl(base_size = 14) +
    theme(legend.position = "top", legend.text = element_text(size = 13))
  
  safe_ggsave("../plots/04_butterfly_violations.png", p, width = 20, height = 14)
  cat("04_butterfly_violations.png saved\n")
  return(p)
}


# Total variance term structure — one line per moneyness level,
# plotted against days to expiry. A violation (w decreasing in T) is
# marked with a red X. Filtered to |k| <= 0.15 to keep the chart
# readable; deep OTM lines add noise without diagnostic value.
plot_calendar <- function() {
  p <- ggplot(
    cal_df %>% filter(abs(log_strike) <= 0.15),
    aes(x     = days_exp,
        y     = total_var,
        group = factor(log_strike),
        color = log_strike)
  ) +
    geom_line(linewidth = 1.1, alpha = 0.9) +
    geom_point(
      data   = cal_df %>% filter(cal_arb_viol, abs(log_strike) <= 0.15),
      aes(x = days_exp, y = total_var),
      color  = RED, size = 5, shape = 4, stroke = 2.5
    ) +
    scale_color_gradientn(
      colors = c(GREEN, ACCENT, TEXT, PURP, RED),
      values = scales::rescale(c(-0.15, -0.05, 0, 0.05, 0.15)),
      name   = "LOG-MONEYNESS k",
      guide  = guide_colorbar(
        barwidth       = 16, barheight = 0.8,
        title.position = "top",
        title.theme    = element_text(color = ACCENT, size = 11,
                                      face = "bold", family = "sans")
      )
    ) +
    scale_x_continuous(breaks = sort(unique(cal_df$days_exp))) +
    labs(
      title    = "CALENDAR SPREAD ARBITRAGE — TOTAL VARIANCE TERM STRUCTURE",
      subtitle = "No-arb: dw/dT >= 0  ·  lines must be monotone increasing  ·  X = violation",
      x        = "Days to Expiry",
      y        = "Total Variance  w(k,T) = σ²(k,T) · T",
      caption  = "Calendar arb: w(k, T₁) > w(k, T₂) for T₁ < T₂"
    ) +
    theme_vsl(base_size = 14) +
    theme(legend.position = "bottom")
  
  safe_ggsave("../plots/05_calendar_arb.png", p, width = 16, height = 10)
  cat("05_calendar_arb.png saved\n")
  return(p)
}


# Interactive 3D residuals surface. Strikes are bucketed to 2.5% bins
# and averaged within each (k, T) cell so isolated noisy contracts
# do not dominate the surface colour. Colorscale is symmetric around
# zero: green = market cheap vs model, red = market rich vs model.
plot_3d_residuals <- function() {
  resid_wide <- resid_df %>%
    mutate(k_bucket = round(log_strike / 0.025) * 0.025) %>%
    group_by(k_bucket, days_exp) %>%
    summarise(mean_resid = mean(residual_pct, na.rm = TRUE), .groups = "drop")
  
  k_vals   <- sort(unique(resid_wide$k_bucket))
  day_vals <- sort(unique(resid_wide$days_exp))
  Z_mat    <- matrix(NA, nrow = length(day_vals), ncol = length(k_vals))
  
  for (i in seq_along(day_vals))
    for (j in seq_along(k_vals)) {
      val <- resid_wide %>%
        filter(days_exp == day_vals[i], k_bucket == k_vals[j]) %>%
        pull(mean_resid)
      if (length(val) > 0) Z_mat[i, j] <- val[1]
    }
  
  fig <- plot_ly() %>%
    add_surface(
      x = k_vals, y = day_vals, z = Z_mat,
      colorscale = list(
        c(0.00, "rgb(0,180,120)"),
        c(0.35, "rgb(0,60,40)"),
        c(0.50, "rgb(4,6,15)"),
        c(0.65, "rgb(60,10,30)"),
        c(1.00, "rgb(255,60,90)")
      ),
      contours = list(
        x = list(show = FALSE),
        y = list(show = FALSE),
        z = list(show = FALSE)
      ),
      lighting = list(
        ambient   = 0.75,
        diffuse   = 0.70,
        roughness = 0.40,
        specular  = 0.60,
        fresnel   = 0.30
      ),
      opacity = 0.96
    ) %>%
    layout(
      title = list(
        text = "SVI RESIDUALS SURFACE  ·  MARKET IV MINUS FIT (%)",
        font = list(color = TEXT, size = 18,
                    family = "system-ui, sans-serif"),
        x = 0.04
      ),
      scene = list(
        xaxis = list(
          title           = list(text = "LOG-MONEYNESS k",
                                 font = list(color = ACCENT, size = 12)),
          backgroundcolor = "#000000",
          gridcolor       = "#1A1F35",
          zerolinecolor   = "#2A3050",
          tickfont        = list(color = MUTED, size = 11)
        ),
        yaxis = list(
          title           = list(text = "DAYS TO EXPIRY",
                                 font = list(color = ACCENT, size = 12)),
          backgroundcolor = "#000000",
          gridcolor       = "#1A1F35",
          zerolinecolor   = "#2A3050",
          tickfont        = list(color = MUTED, size = 11)
        ),
        zaxis = list(
          title           = list(text = "RESIDUAL %",
                                 font = list(color = ACCENT, size = 12)),
          backgroundcolor = "#000000",
          gridcolor       = "#1A1F35",
          zerolinecolor   = "#2A3050",
          tickfont        = list(color = MUTED, size = 11)
        ),
        bgcolor     = "#000000",
        camera      = list(eye = list(x = 1.7, y = -1.7, z = 1.2)),
        aspectmode  = "manual",
        aspectratio = list(x = 1.2, y = 1.2, z = 0.8)
      ),
      paper_bgcolor = BG,
      font   = list(color = MUTED, size = 11,
                    family = "system-ui, sans-serif"),
      margin = list(l = 0, r = 0, t = 60, b = 0)
    )
  
  safe_savewidget(fig, "../plots/06_residuals_3d.html")
  cat("06_residuals_3d.html saved\n")
  return(fig)
}


# Run all plots when this script is executed directly. The shiny_running
# guard prevents this block from firing when the Shiny app sources the
# file — the app calls each plot function individually on demand.
if (!exists("shiny_running")) {
  cat("Generating all visualisations...\n\n")
  plot_3d_surface()
  plot_smiles()
  plot_arb_heatmap()
  plot_butterfly()
  plot_calendar()
  plot_3d_residuals()
  cat("\nAll plots saved to ../plots/\n")
}
