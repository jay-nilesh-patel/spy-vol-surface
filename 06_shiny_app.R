# 06_shiny_app.R
# Shiny dashboard for the SPY Implied Volatility Surface Lab.
# Sources 05_visualisations.R for plot functions and the shared colour
# palette. The shiny_running flag prevents the standalone plot-export
# block in 05_visualisations.R from firing when this app is launched.
#
# Tabs:
#   IV Surface   — interactive 3D SVI surface with IV cap + expiry mode
#   Smile        — per-expiry vol smile: SVI fit vs market IV
#   Arb Heatmap  — composite arb score across (log-moneyness, expiry)
#   Butterfly    — risk-neutral density g(k) violation diagnostic
#   Calendar     — total variance term structure monotonicity check
#   Residuals    — 3D market IV minus SVI fit surface
#   Summary      — per-expiry IV stats table
#
# Inputs  : ../data/iv_surface.rds   (from 02_iv_calculation.R)
#           ../data/arb_results.rds  (from 04_arb_detection.R)
#           ../data/svi_fits.rds     (from 03_svi_fit.R)

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)

source("05_visualisations.R", local = TRUE)
shiny_running <- TRUE


# ── UI ──────────────────────────────────────────────────────────────

ui <- fluidPage(
  
  title = "Vol Surface Lab",
  
  tags$head(tags$style(HTML("

    *, body, html { box-sizing: border-box; }
    body, html {
      background-color: #000000 !important;
      color: #E4ECFF !important;
      font-family: system-ui, -apple-system, BlinkMacSystemFont,
                   'Segoe UI', sans-serif !important;
      margin: 0; padding: 0;
      overflow-y: auto !important;
    }

    .vs-topbar {
      background-color: #000000;
      border-bottom: 1px solid #1C2033;
      padding: 14px 28px;
      display: flex; align-items: center;
      justify-content: space-between;
    }
    .vs-top-left  { display: flex; flex-direction: column; }
    .vs-top-title {
      font-size: 15px; font-weight: 600;
      letter-spacing: 0.18em; text-transform: uppercase;
      color: #E4ECFF; line-height: 1;
    }
    .vs-top-sub {
      font-size: 10.5px; color: #6B7491;
      margin-top: 4px; letter-spacing: 0.04em;
    }
    .vs-top-stats { display: flex; gap: 0; align-items: center; }
    .vs-stat {
      display: flex; flex-direction: column; align-items: flex-end;
      padding: 0 18px; border-right: 1px solid #1C2033;
    }
    .vs-stat:last-child { border-right: none; padding-right: 0; }
    .vs-stat-label {
      font-size: 9.5px; text-transform: uppercase;
      letter-spacing: 0.14em; color: #6B7491; line-height: 1;
    }
    .vs-stat-value {
      font-size: 14px; font-weight: 600; color: #E4ECFF;
      font-family: ui-monospace, 'SF Mono', Menlo, Consolas, monospace;
      line-height: 1.4;
    }
    .vs-stat-value.accent { color: #4CA3FF; }

    .nav-tabs {
      background-color: #000000 !important;
      border-bottom: 1px solid #1C2033 !important;
      padding: 0 20px; margin-bottom: 0 !important;
    }
    .nav-tabs > li > a {
      color: #6B7491 !important;
      background-color: transparent !important;
      border: none !important; border-radius: 0 !important;
      padding: 11px 18px !important;
      font-size: 11px !important; font-weight: 500 !important;
      text-transform: uppercase; letter-spacing: 0.13em;
    }
    .nav-tabs > li > a:hover        { color: #C4CFED !important;
                                      background-color: transparent !important; }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      color: #E4ECFF !important;
      background-color: transparent !important;
      border: none !important;
      border-bottom: 2px solid #4CA3FF !important;
    }
    .tab-content { background-color: #000000 !important; }

    .vs-panel {
      margin: 20px 24px;
      background-color: #04060F;
      border: 1px solid #1C2033;
      border-radius: 4px; overflow: hidden;
    }
    .vs-panel-header {
      padding: 14px 20px 12px 20px;
      border-bottom: 1px solid #1C2033;
      display: flex; align-items: baseline;
      justify-content: space-between;
    }
    .vs-panel-title {
      font-size: 12px; font-weight: 600;
      text-transform: uppercase; letter-spacing: 0.15em; color: #E4ECFF;
    }
    .vs-panel-sub  { font-size: 10.5px; color: #6B7491; margin-top: 3px; }
    .vs-panel-tag  {
      font-size: 10px; text-transform: uppercase; letter-spacing: 0.12em;
      color: #4CA3FF;
      font-family: ui-monospace, 'SF Mono', Menlo, Consolas, monospace;
      background: #04111F; border: 1px solid #0D2540;
      padding: 3px 8px; border-radius: 2px;
    }
    .vs-panel-body { padding: 16px 18px 20px 18px; }

    .vs-controls {
      display: flex; gap: 24px;
      margin-bottom: 14px;
      align-items: flex-end; flex-wrap: wrap;
    }
    .vs-ctrl-label {
      font-size: 11px; color: #6B7491;
      text-transform: uppercase; letter-spacing: 0.12em; margin-bottom: 4px;
    }
    .form-control, select {
      background-color: #020308 !important;
      border: 1px solid #1C2033 !important;
      color: #E4ECFF !important; font-size: 12px !important;
    }
    .form-control:focus {
      border-color: #4CA3FF !important;
      box-shadow: 0 0 0 1px #4CA3FF !important;
      outline: none !important;
    }

    .vs-radio .radio label {
      color: #6B7491 !important; font-size: 11px !important;
      text-transform: uppercase; letter-spacing: 0.1em; cursor: pointer;
    }
    .vs-radio .radio input[type=radio]:checked + label,
    .vs-radio .radio label:hover { color: #E4ECFF !important; }

    table { width: 100% !important; border-collapse: collapse !important; }
    thead tr th {
      background-color: #04060F !important; color: #F0B35A !important;
      font-size: 11px !important; text-transform: uppercase;
      letter-spacing: 0.1em; padding: 10px 14px !important;
      border-bottom: 1px solid #1C2033 !important;
      border-right:  1px solid #1C2033 !important; font-weight: 600 !important;
    }
    tbody tr td {
      background-color: #000000 !important;
      border-bottom: 1px solid #0E1120 !important;
      border-right:  1px solid #0E1120 !important;
      color: #E4ECFF !important; font-size: 12px !important;
      padding: 8px 14px !important;
      font-family: ui-monospace, 'SF Mono', Menlo, Consolas, monospace;
    }
    tbody tr:hover td { background-color: #04060F !important; }

    ::-webkit-scrollbar       { width: 6px; height: 6px; }
    ::-webkit-scrollbar-track { background: #000000; }
    ::-webkit-scrollbar-thumb { background: #1C2033; border-radius: 3px; }
    ::-webkit-scrollbar-thumb:hover { background: #2A304A; }

  "))),
  
  div(class = "vs-topbar",
      div(class = "vs-top-left",
          div(class = "vs-top-title", "VOL SURFACE LAB"),
          div(class = "vs-top-sub",
              "SPY  \u00b7  SVI Parametric Model  \u00b7  No-Arb Diagnostics  \u00b7  Yahoo Finance")
      ),
      uiOutput("topbar_stats")
  ),
  
  tabsetPanel(
    id = "main_tabs", type = "tabs",
    
    tabPanel("IV Surface",
             div(class = "vs-panel",
                 div(class = "vs-panel-header",
                     div(
                       div(class = "vs-panel-title", "3D Implied Volatility Surface"),
                       div(class = "vs-panel-sub",
                           "Hover, rotate, zoom  \u00b7  all expiries or single expiry mode")
                     ),
                     div(class = "vs-panel-tag", "SVI FIT")
                 ),
                 div(class = "vs-panel-body",
                     div(class = "vs-controls",
                         div(
                           div(class = "vs-ctrl-label", "View Mode"),
                           div(class = "vs-radio",
                               radioButtons("surface_mode", label = NULL,
                                            choices  = c("All Expiries" = "all",
                                                         "Single Expiry" = "single"),
                                            selected = "all", inline = TRUE
                               )
                           )
                         ),
                         conditionalPanel(
                           condition = "input.surface_mode == 'single'",
                           div(
                             div(class = "vs-ctrl-label", "Expiry"),
                             selectInput("surface_expiry", label = NULL,
                                         choices = NULL, width = "160px")
                           )
                         ),
                         div(
                           div(class = "vs-ctrl-label", "IV Cap (%)"),
                           sliderInput("iv_cap", label = NULL,
                                       min = 40, max = 200, value = 80,
                                       step = 5, width = "200px")
                         )
                     ),
                     plotlyOutput("plot_3d_surface", height = "800px")
                 )
             )
    ),
    
    tabPanel("Smile",
             div(class = "vs-panel",
                 div(class = "vs-panel-header",
                     div(
                       div(class = "vs-panel-title", "Volatility Smile by Expiry"),
                       div(class = "vs-panel-sub",
                           "Select expiry  \u00b7  hover for IV + residual  \u00b7  zoom & pan")
                     ),
                     div(class = "vs-panel-tag", "MARKET vs MODEL")
                 ),
                 div(class = "vs-panel-body",
                     div(class = "vs-controls",
                         div(
                           div(class = "vs-ctrl-label", "Expiry"),
                           selectInput("smile_expiry", label = NULL,
                                       choices = NULL, width = "160px")
                         )
                     ),
                     plotlyOutput("plot_smiles", height = "650px")
                 )
             )
    ),
    
    tabPanel("Arb Heatmap",
             div(class = "vs-panel",
                 div(class = "vs-panel-header",
                     div(
                       div(class = "vs-panel-title", "Volatility Arbitrage Heatmap"),
                       div(class = "vs-panel-sub",
                           "Score = |market IV \u2212 SVI fit| %  \u00b7  hover for exact values")
                     ),
                     div(class = "vs-panel-tag", "MISPRICING SCORE")
                 ),
                 div(class = "vs-panel-body",
                     plotlyOutput("plot_arb_heatmap", height = "750px")
                 )
             )
    ),
    
    tabPanel("Butterfly",
             div(class = "vs-panel",
                 div(class = "vs-panel-header",
                     div(
                       div(class = "vs-panel-title",
                           "Butterfly Arb \u2014 Risk-Neutral Density g(k)"),
                       div(class = "vs-panel-sub",
                           "g(k) < 0 = negative density = violation  \u00b7  free y-scale per panel")
                     ),
                     div(class = "vs-panel-tag", "BREEDEN-LITZENBERGER")
                 ),
                 div(class = "vs-panel-body",
                     plotOutput("plot_butterfly", height = "950px")
                 )
             )
    ),
    
    tabPanel("Calendar",
             div(class = "vs-panel",
                 div(class = "vs-panel-header",
                     div(
                       div(class = "vs-panel-title",
                           "Calendar Spread Arb \u2014 Total Variance Term Structure"),
                       div(class = "vs-panel-sub",
                           "dw/dT \u2265 0 required  \u00b7  X = calendar arb violation")
                     ),
                     div(class = "vs-panel-tag", "NO-ARB CONDITION")
                 ),
                 div(class = "vs-panel-body",
                     plotOutput("plot_calendar", height = "780px")
                 )
             )
    ),
    
    tabPanel("Residuals",
             div(class = "vs-panel",
                 div(class = "vs-panel-header",
                     div(
                       div(class = "vs-panel-title", "SVI Residuals Surface"),
                       div(class = "vs-panel-sub",
                           "Market IV minus SVI fit (%)  \u00b7  3D interactive")
                     ),
                     div(class = "vs-panel-tag", "RESIDUALS")
                 ),
                 div(class = "vs-panel-body",
                     plotlyOutput("plot_3d_residuals", height = "800px")
                 )
             )
    ),
    
    tabPanel("Summary",
             div(class = "vs-panel",
                 div(class = "vs-panel-header",
                     div(
                       div(class = "vs-panel-title", "IV Surface Summary by Expiry"),
                       div(class = "vs-panel-sub",
                           "ATM IV  \u00b7  put skew  \u00b7  call skew  \u00b7  contract counts")
                     ),
                     div(class = "vs-panel-tag", "IV SUMMARY")
                 ),
                 div(class = "vs-panel-body",
                     tableOutput("summary_table")
                 )
             )
    )
  )
)


# ── Server ──────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  iv_data <- tryCatch(readRDS("../data/iv_surface.rds"), error = function(e) NULL)
  
  # Populate dropdowns with actual calendar dates.
  # setNames(value, label): value = character days_exp (used internally
  # by all filter calls via as.numeric()), label = "DD/MM/YYYY" shown
  # in the dropdown. Must be character not numeric — Shiny drops labels
  # on numeric named vectors.
  observe({
    if (is.null(iv_data)) return(NULL)
    
    map <- iv_data$expiry_date_map
    
    if (!is.null(map) && nrow(map) > 0) {
      date_choices <- setNames(
        as.character(map$days_exp),
        map$expiry_label
      )
    } else {
      days_choices <- sort(unique(iv_data$iv_df$days_exp))
      date_choices <- setNames(
        as.character(days_choices),
        paste0(days_choices, "d")
      )
    }
    
    updateSelectInput(session, "smile_expiry",
                      choices  = date_choices,
                      selected = date_choices[1])
    updateSelectInput(session, "surface_expiry",
                      choices  = date_choices,
                      selected = date_choices[1])
  })
  
  # Top bar stats
  output$topbar_stats <- renderUI({
    tryCatch({
      if (is.null(iv_data)) stop("no data")
      S0     <- iv_data$spot
      n_tot  <- nrow(iv_data$iv_df)
      n_exp  <- length(unique(iv_data$iv_df$days_exp))
      fetch  <- format(iv_data$fetch_time, "%H:%M")
      atm_iv <- mean(
        iv_data$iv_df$iv[abs(iv_data$iv_df$log_strike) < 0.05],
        na.rm = TRUE
      )
      div(class = "vs-top-stats",
          div(class = "vs-stat",
              div(class = "vs-stat-label", "SPOT"),
              div(class = "vs-stat-value accent", paste0("$", round(S0, 2)))),
          div(class = "vs-stat",
              div(class = "vs-stat-label", "ATM IV"),
              div(class = "vs-stat-value", paste0(round(atm_iv * 100, 1), "%"))),
          div(class = "vs-stat",
              div(class = "vs-stat-label", "EXPIRIES"),
              div(class = "vs-stat-value", n_exp)),
          div(class = "vs-stat",
              div(class = "vs-stat-label", "CONTRACTS"),
              div(class = "vs-stat-value", n_tot)),
          div(class = "vs-stat",
              div(class = "vs-stat-label", "FETCHED"),
              div(class = "vs-stat-value", fetch))
      )
    }, error = function(e) {
      div(style = "color:#6B7491; font-size:11px;",
          "DATA NOT LOADED \u2014 run run_all.R first")
    })
  })
  
  # 3D IV surface — rebuilds on iv_cap or surface_mode change.
  # Single expiry mode duplicates the matrix into two rows so Plotly
  # renders it as a flat ribbon instead of refusing a 1-row surface.
  output$plot_3d_surface <- renderPlotly({
    req(input$iv_cap, input$surface_mode)
    
    iv_cap_val <- input$iv_cap / 100
    k_grid     <- seq(-0.30, 0.30, length.out = 120)
    
    fits_use <- if (input$surface_mode == "single") {
      req(input$surface_expiry)
      sel_days <- as.numeric(input$surface_expiry)
      fits[sapply(fits, function(f) f$days == sel_days)]
    } else {
      fits
    }
    
    exp_sorted <- names(fits_use)[order(sapply(fits_use, function(f) f$T))]
    days_vec   <- sapply(fits_use[exp_sorted], function(f) f$days)
    
    iv_matrix <- matrix(NA, nrow = length(days_vec), ncol = length(k_grid))
    for (i in seq_along(exp_sorted)) {
      fit <- fits_use[[exp_sorted[i]]]
      for (j in seq_along(k_grid)) {
        w <- svi_w(k_grid[j], fit$params)
        iv_matrix[i, j] <- sqrt(max(w / fit$T, 0))
      }
    }
    
    iv_matrix <- pmin(pmax(iv_matrix, 0), iv_cap_val)
    
    if (input$surface_mode == "single" && length(days_vec) == 1) {
      iv_matrix <- rbind(iv_matrix, iv_matrix)
      days_vec  <- c(days_vec[1] - 0.5, days_vec[1] + 0.5)
    }
    
    plot_ly() %>%
      add_surface(
        x = k_grid, y = days_vec, z = iv_matrix,
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
        lighting = list(ambient = 0.7, diffuse = 0.85,
                        roughness = 0.3, specular = 0.6, fresnel = 0.3),
        opacity = 0.97,
        hovertemplate = "k = %{x:.3f}<br>Days = %{y}<br>IV = %{z:.1%}<extra></extra>"
      ) %>%
      layout(
        title = list(
          text = paste0(
            "SPY IV SURFACE  \u00b7  SVI  \u00b7  SPOT $", round(S0, 2),
            if (input$surface_mode == "single")
              paste0("  \u00b7  ", input$surface_expiry, "d EXPIRY") else ""
          ),
          font = list(color = TEXT, size = 16,
                      family = "system-ui, sans-serif"),
          x = 0.02
        ),
        scene = list(
          xaxis = list(
            title           = list(text = "LOG-MONEYNESS k",
                                   font = list(color = ACCENT, size = 12)),
            backgroundcolor = "#000000", gridcolor = "#1A1F35",
            zerolinecolor   = "#2A3050",
            tickfont        = list(color = MUTED, size = 11),
            tickformat      = ".2f"
          ),
          yaxis = list(
            title           = list(text = "DAYS TO EXPIRY",
                                   font = list(color = ACCENT, size = 12)),
            backgroundcolor = "#000000", gridcolor = "#1A1F35",
            zerolinecolor   = "#2A3050",
            tickfont        = list(color = MUTED, size = 11)
          ),
          zaxis = list(
            title           = list(text = "IMPLIED VOL",
                                   font = list(color = ACCENT, size = 12)),
            backgroundcolor = "#000000", gridcolor = "#1A1F35",
            zerolinecolor   = "#2A3050",
            tickfont        = list(color = MUTED, size = 11),
            tickformat      = ".0%"
          ),
          bgcolor     = "#000000",
          camera      = list(eye = list(x = 1.7, y = -1.7, z = 1.1)),
          aspectmode  = "manual",
          aspectratio = list(x = 1.2, y = 1.2, z = 0.8)
        ),
        paper_bgcolor = BG, plot_bgcolor = BG,
        font   = list(color = MUTED, size = 11,
                      family = "system-ui, sans-serif"),
        margin = list(l = 0, r = 0, t = 60, b = 0)
      )
  })
  
  # Per-expiry smile — SVI fit as amber line, market IV as dots
  # coloured by residual_pct (green = cheap, red = rich vs model).
  output$plot_smiles <- renderPlotly({
    req(iv_data, input$smile_expiry)
    sel    <- as.numeric(input$smile_expiry)
    df_fit <- fitted_df %>% filter(days_exp == sel)
    df_mkt <- resid_df  %>% filter(days_exp == sel)
    
    plot_ly() %>%
      add_lines(
        data = df_fit, x = ~log_strike, y = ~iv_fit,
        name = "SVI Fit",
        line = list(color = GOLD, width = 3),
        hovertemplate = "k = %{x:.3f}<br>SVI IV = %{y:.1%}<extra>SVI Fit</extra>"
      ) %>%
      add_markers(
        data = df_mkt, x = ~log_strike, y = ~iv,
        name = "Market IV",
        marker = list(
          size  = 9,
          color = ~residual_pct,
          colorscale = list(
            c(0,   "rgb(46,204,154)"),
            c(0.5, "rgb(228,236,255)"),
            c(1.0, "rgb(255,77,109)")
          ),
          showscale = TRUE,
          colorbar  = list(
            title       = "Residual %",
            titlefont   = list(color = ACCENT, size = 11),
            tickfont    = list(color = MUTED,  size = 10),
            bgcolor     = BG, bordercolor = BORDER,
            len = 0.6, thickness = 14
          ),
          line = list(color = BG, width = 0.5)
        ),
        hovertemplate = paste0(
          "k = %{x:.3f}<br>Market IV = %{y:.1%}<br>",
          "Residual = %{marker.color:.2f}%<extra>Market IV</extra>"
        )
      ) %>%
      layout(
        title = list(
          text = paste0("VOL SMILE  \u00b7  ", sel,
                        " DAYS  \u00b7  SPOT $", round(S0, 2)),
          font = list(color = TEXT, size = 16,
                      family = "system-ui, sans-serif"),
          x = 0.02
        ),
        xaxis = list(
          title      = "Log-Moneyness  k = log(K / S)",
          titlefont  = list(color = ACCENT, size = 12),
          tickfont   = list(color = MUTED,  size = 11),
          gridcolor  = GRID, zerolinecolor = BORDER,
          tickformat = ".2f"
        ),
        yaxis = list(
          title      = "Implied Volatility",
          titlefont  = list(color = ACCENT, size = 12),
          tickfont   = list(color = MUTED,  size = 11),
          gridcolor  = GRID, zerolinecolor = BORDER,
          tickformat = ".0%"
        ),
        legend        = list(font = list(color = TEXT, size = 11),
                             bgcolor = PANEL, bordercolor = BORDER),
        paper_bgcolor = BG, plot_bgcolor = PANEL,
        margin        = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  # Arb heatmap — scores bucketed to 2% moneyness bins.
  # Text colour flips to black on very bright tiles (score > 18).
  output$plot_arb_heatmap <- renderPlotly({
    heatmap_df <- arb_score_df %>%
      mutate(strike_bucket = round(log_strike / 0.02) * 0.02) %>%
      group_by(strike_bucket, days_exp) %>%
      summarise(arb_score = mean(arb_score, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        x_label    = sprintf("%+.0f%%", strike_bucket * 100),
        y_label    = paste0(days_exp, "d"),
        label_text = sprintf("%.1f%%", arb_score),
        hover_text = sprintf(
          "Moneyness: %+.0f%%<br>Days: %d<br>Arb Score: %.1f%%",
          strike_bucket * 100, days_exp, arb_score
        ),
        text_color = ifelse(arb_score > 18, "#000000", "#FFFFFF")
      )
    
    k_vals   <- sort(unique(heatmap_df$strike_bucket))
    day_vals <- sort(unique(heatmap_df$days_exp))
    x_labels <- sprintf("%+.0f%%", k_vals * 100)
    y_labels <- paste0(day_vals, "d")
    
    Z_mat <- matrix(NA, nrow = length(day_vals), ncol = length(k_vals))
    H_mat <- matrix("",  nrow = length(day_vals), ncol = length(k_vals))
    for (i in seq_along(day_vals))
      for (j in seq_along(k_vals)) {
        val <- heatmap_df %>%
          filter(days_exp == day_vals[i], strike_bucket == k_vals[j])
        if (nrow(val) > 0) {
          Z_mat[i, j] <- val$arb_score[1]
          H_mat[i, j] <- val$hover_text[1]
        }
      }
    
    plot_ly() %>%
      add_heatmap(
        x = x_labels, y = y_labels, z = Z_mat,
        text = H_mat,
        hovertemplate = "%{text}<extra></extra>",
        colorscale = list(
          c(0.00, PANEL),     c(0.08, "#0A1830"),
          c(0.20, "#0D3060"), c(0.35, "#1060A0"),
          c(0.50, "#4CA3FF"), c(0.65, "#F0B35A"),
          c(0.80, "#FF8C42"), c(1.00, "#FF4D6D")
        ),
        colorbar = list(
          title       = "Arb Score %",
          titlefont   = list(color = ACCENT, size = 11),
          tickfont    = list(color = MUTED,  size = 10),
          bgcolor     = BG, bordercolor = BORDER, thickness = 14
        ),
        showscale = TRUE
      ) %>%
      add_trace(
        data       = heatmap_df,
        x          = ~x_label, y = ~y_label,
        text       = ~label_text,
        textfont   = list(color = ~text_color, size = 11,
                          family = "ui-monospace, SF Mono, Menlo, monospace"),
        mode       = "text", type = "scatter",
        hoverinfo  = "skip", showlegend = FALSE
      ) %>%
      layout(
        title = list(
          text = "VOLATILITY ARBITRAGE HEATMAP  \u00b7  SPY",
          font = list(color = TEXT, size = 16,
                      family = "system-ui, sans-serif"),
          x = 0.02
        ),
        xaxis = list(
          title         = "Log-Moneyness  k = log(K / S)",
          titlefont     = list(color = ACCENT, size = 12),
          tickfont      = list(color = MUTED,  size = 11),
          gridcolor     = GRID, type = "category",
          categoryorder = "array", categoryarray = x_labels
        ),
        yaxis = list(
          title         = "Days to Expiry",
          titlefont     = list(color = ACCENT, size = 12),
          tickfont      = list(color = MUTED,  size = 11),
          gridcolor     = GRID, type = "category",
          categoryorder = "array", categoryarray = y_labels
        ),
        paper_bgcolor = BG, plot_bgcolor = PANEL,
        margin        = list(l = 60, r = 40, t = 60, b = 60)
      )
  })
  
  output$plot_butterfly    <- renderPlot({ plot_butterfly()    }, res = 120)
  output$plot_calendar     <- renderPlot({ plot_calendar()     }, res = 120)
  output$plot_3d_residuals <- renderPlotly({ plot_3d_residuals() })
  
  # Summary table — IV columns as %, call_skew NA shown as em-dash,
  # expiry_label column included so actual dates appear in the table.
  output$summary_table <- renderTable({
    tryCatch({
      iv_data$summary %>%
        mutate(
          atm_iv    = paste0(round(atm_iv    * 100, 1), "%"),
          min_iv    = paste0(round(min_iv    * 100, 1), "%"),
          max_iv    = paste0(round(max_iv    * 100, 1), "%"),
          put_skew  = paste0(round(put_skew  * 100, 2), "%"),
          call_skew = ifelse(is.na(call_skew), "\u2014",
                             paste0(round(call_skew * 100, 2), "%"))
        ) %>%
        rename(
          `Days`      = days_exp,
          `Expiry`    = expiry_label,
          `N Total`   = n,
          `N Calls`   = n_calls,
          `N Puts`    = n_puts,
          `ATM IV`    = atm_iv,
          `Min IV`    = min_iv,
          `Max IV`    = max_iv,
          `Put Skew`  = put_skew,
          `Call Skew` = call_skew
        )
    }, error = function(e) {
      data.frame(Error = paste("Could not load:", e$message))
    })
  },
  striped = FALSE, bordered = FALSE,
  hover = TRUE, na = "\u2014", width = "100%"
  )
}


shinyApp(ui = ui, server = server)
