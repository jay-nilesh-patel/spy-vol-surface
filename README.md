# SPY Volatility Surface Lab

An end-to-end implied volatility surface analyser for SPY options, built in R with a live Shiny dashboard. Fetches real-time options data from Yahoo Finance, computes BSM implied volatilities, fits Gatheral's SVI model to each expiry slice, and runs three classes of no-arbitrage diagnostics — all visualised in an interactive dark-themed UI.

---

## Features

- Live SPY options chain via `yfinance` (Python) through `reticulate`
- BSM implied volatility solved numerically per contract using `uniroot`
- SVI parametric surface fit (Gatheral 2004) per expiry slice
- Three arbitrage diagnostic engines: butterfly, calendar spread, model residuals
- Interactive Plotly 3D surface with IV cap control and single/all expiry mode
- Per-expiry vol smile overlay: SVI fit vs market IV coloured by residual
- Arbitrage heatmap across log-moneyness and expiry dimensions
- Risk-neutral density plot with violation regions shaded
- Total variance term structure with calendar arb markers

---

## Project Structure

vol-surface-lab/
├── R/
│ ├── 01_data_fetch.R # yfinance data pull via reticulate
│ ├── 02_iv_calculation.R # BSM IV solver, OTM filter, quality filter
│ ├── 03_svi_fit.R # SVI calibration per expiry slice
│ ├── 04_arb_detection.R # butterfly, calendar, residual diagnostics
│ ├── 05_visualisations.R # all ggplot2 + Plotly plot functions
│ ├── 06_shiny_app.R # Shiny dashboard
│ ├── run_all.R # single entry point
│ └── data/ # generated — not tracked in git
└── plots/ # generated — not tracked in git

text

---

## Quickstart

```r
# Install R dependencies
install.packages(c("shiny", "plotly", "ggplot2", "dplyr", "readr",
                   "tidyr", "scales", "htmlwidgets", "nloptr",
                   "reticulate", "lubridate", "purrr"))

# Install Python dependency
# pip install yfinance pandas numpy

# Run full pipeline and launch dashboard
source("R/run_all.R")

```

How It Works
1. Data Fetch
Live SPY options chain is pulled from Yahoo Finance via yfinance (Python) using reticulate. The nearest 8 expiries are fetched, strikes are filtered to within 30% of spot, and only contracts with valid two-sided markets (bid > 0, ask > 0, mid > 0.01) are retained.

2. Implied Volatility
For each contract, the Black-Scholes-Merton model is inverted numerically using uniroot to find the implied volatility that matches the market mid price. Contracts with IVs below 2% or above 250% are discarded as data errors. Only OTM options are used — calls above ATM and puts below ATM — as they are more liquid and carry cleaner information about the risk-neutral distribution.

3. SVI Surface Fit
Gatheral's Stochastic Volatility Inspired (SVI) model is fitted to each expiry slice independently. SVI fits total variance as a smooth parametric function of log-moneyness using five parameters controlling the overall variance level, wing slope, skew direction, ATM location, and ATM curvature. Calibration minimises sum of squared errors in total variance space using the derivative-free Subplex algorithm via nloptr, with three random starting points per slice to avoid local minima.

4. Butterfly Arbitrage
A butterfly arbitrage exists when the risk-neutral density of the underlying becomes negative. Using the Breeden-Litzenberger result, the risk-neutral density is computed from the second derivative of call prices with respect to strike. The Gatheral-Jacquier density condition is evaluated on a 500-point grid per expiry slice. Any region where the density turns negative is flagged and shaded in the Butterfly tab.

5. Calendar Spread Arbitrage
For any fixed strike, total variance must increase with time to expiry — otherwise a calendar spread (sell near, buy far) generates a riskless profit. This monotonicity condition is checked across 21 moneyness points per expiry pair. Violations are marked with an X on the Calendar tab term structure plot.

6. Model Residuals
For each market contract, the percentage difference between market IV and SVI fitted IV is computed. Contracts more than 3% away from the model are flagged. A composite arb score adds a bonus to flagged contracts so they stand out in the heatmap colour scale.

7. Skew Metrics
Put skew and call skew are computed per expiry as the average IV differential between OTM puts or calls and the ATM level. For SPY, put skew is persistently positive (crash protection is bid up) and call skew is typically negative (calls are cheap relative to puts).

## References

- Gatheral, J. (2004). *A parsimonious arbitrage-free implied volatility parametrization with application to the valuation of volatility derivatives.* Global Derivatives & Risk Management, Madrid.
- Breeden, D. T., & Litzenberger, R. H. (1978). *Prices of state-contingent claims implicit in option prices.* Journal of Business, 51(4), 621–651.
- Black, F., & Scholes, M. (1973). *The pricing of options and corporate liabilities.* Journal of Political Economy, 81(3), 637–654.
- Gatheral, J., & Jacquier, A. (2014). *Arbitrage-free SVI volatility surfaces.* Quantitative Finance, 14(1), 59–71.
