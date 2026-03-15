# run_all.R
# Single entry point for the SPY Vol Surface pipeline.
# Sources each script in order, then launches the Shiny dashboard.
#
# Usage (from RStudio): open this file and press Ctrl+Shift+Enter, or
#   source("R/run_all.R") from the project root in the R console.
#
# Script execution order and what each step produces:
#   01_fetch_data.R       -> data/options_raw.csv, data/options_data.rds
#   02_iv_calculation.R   -> data/iv_surface.rds, data/iv_surface.csv
#   03_svi_calibration.R  -> data/svi_fits.rds, data/svi_params.rds
#   04_arb_detection.R    -> data/arb_results.rds
#   06_shiny_app.R        -> launches interactive dashboard

# Set working directory to the R/ folder so all relative paths in the
# sourced scripts resolve correctly regardless of where the project
# root is on the user's machine.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

cat("SPY Vol Surface Pipeline starting\n\n")

cat("Step 1/4 — Fetching options data\n")
source("01_data_fetch.R")

cat("\nStep 2/4 — Calculating implied volatilities\n")
source("02_iv_calculation.R")

cat("\nStep 3/4 — Fitting SVI surface\n")
source("03_svi_fit.R")

cat("\nStep 4/4 — Detecting arbitrage\n")
source("04_arb_detection.R")

cat("\nPipeline complete — launching dashboard\n\n")
shiny::runApp("06_shiny_app.R")
