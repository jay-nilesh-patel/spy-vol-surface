# spy-vol-surface
End-to-end SPY options volatility surface analyser. Fetches live options chain via yfinance, computes BSM implied volatilities using numerical root-finding, fits Gatheral's SVI model per expiry slice, and runs butterfly + calendar spread + residual arbitrage diagnostics. Interactive Shiny dashboard with 3D Plotly surface. Built in R.
