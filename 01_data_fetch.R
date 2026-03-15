# 01_data_fetch.R
# Fetches SPY options chain from Yahoo Finance via yfinance (Python).
# Saves raw contracts + spot price to data/ for downstream scripts.
#
# Dependencies : reticulate, yfinance (pip install yfinance)
# Outputs      : data/options_raw.csv
#                data/options_data.rds  (list: options, spot,
#                                              fetch_time, expiry_date_map)

library(reticulate)
library(dplyr)
library(readr)
library(lubridate)

# Point reticulate to the local Python installation.
# Change this path if Python lives somewhere else on your machine.
Sys.setenv(RETICULATE_PYTHON = "C:/Python314/python.exe")
use_python("C:/Python314/python.exe", required = TRUE)


# Downloads the options chain for a given ticker, cleans it, and
# writes the result to disk. Returns a named list with the clean
# data frame, spot price, fetch timestamp, and an expiry date map
# (days_exp -> "DD/MM/YYYY") used by the Shiny UI dropdowns.
fetch_spy_options <- function(ticker = "SPY") {
  
  cat("Fetching", ticker, "options chain via yfinance...\n")
  
  # Build the Python script as a plain string using paste0 so that
  # Python f-string braces and strptime % characters are not
  # misinterpreted by R's sprintf formatter.
  python_code <- paste0("
import yfinance as yf
import pandas as pd
import numpy as np
from datetime import date, datetime

ticker_str = '", ticker, "'
ticker_obj = yf.Ticker(ticker_str)

# Current spot from the most recent daily close
hist = ticker_obj.history(period='1d')
spot = float(hist['Close'].iloc[-1])
print(f'Spot: {spot:.2f}')

expiries = ticker_obj.options
today    = date.today()
print(f'Expiries available: {len(expiries)}')

all_rows = []

for exp in expiries[:8]:
    try:
        days = (datetime.strptime(exp, '%Y-%m-%d').date() - today).days
        T    = days / 365.0

        # Skip expiries less than 3 calendar days out —
        # gamma blows up and SVI fitting becomes unstable
        if T < 3 / 365:
            continue

        chain = ticker_obj.option_chain(exp)

        for opt_type, df in [('call', chain.calls), ('put', chain.puts)]:
            df               = df.copy()
            df['type']       = opt_type
            df['expiry']     = exp
            df['T']          = T
            df['spot']       = spot
            df['mid_price']  = (df['bid'] + df['ask']) / 2
            df['log_strike'] = np.log(df['strike'] / spot)
            df['days_exp']   = round(T * 365)
            df['ticker']     = ticker_str
            all_rows.append(df)

        print(f'  OK  {exp}  ({days} days)')

    except Exception as e:
        print(f'  Skipped {exp}: {e}')

options_df = pd.concat(all_rows, ignore_index=True)

# Keep only contracts with valid two-sided markets and
# strikes within +/-30% of spot
options_df = options_df[
    (options_df['bid']       > 0)          &
    (options_df['ask']       > 0)          &
    (options_df['mid_price'] > 0.01)       &
    (options_df['strike']    > spot * 0.70) &
    (options_df['strike']    < spot * 1.30)
].copy()

options_df['days_exp'] = options_df['days_exp'].astype(int)
print(f'Valid contracts after filtering: {len(options_df)}')
")
  
  py_run_string(python_code)
  
  # Pull results from Python into R
  S0         <- py$spot
  options_df <- py$options_df
  
  cat("Spot:", round(S0, 2), "\n")
  cat("Contracts fetched:", nrow(options_df), "\n")
  
  # Rename to consistent column names used across all scripts
  final_df <- options_df %>%
    rename(
      Strike = strike,
      Bid    = bid,
      Ask    = ask,
      Last   = lastPrice,
      Vol    = volume,
      OI     = openInterest
    ) %>%
    select(
      ticker, expiry, T, type,
      Strike, Bid, Ask, Last, Vol, OI,
      mid_price, spot, log_strike, days_exp
    ) %>%
    mutate(
      expiry   = as.Date(expiry),
      days_exp = as.integer(days_exp),
      Strike   = as.numeric(Strike),
      Bid      = as.numeric(Bid),
      Ask      = as.numeric(Ask),
      Last     = as.numeric(Last),
      Vol      = as.numeric(Vol),
      OI       = as.numeric(OI)
    ) %>%
    filter(
      !is.na(log_strike),
      !is.na(Bid), !is.na(Ask),
      !is.na(Strike)
    )
  
  cat("Clean contracts after NA removal:", nrow(final_df), "\n")
  
  # Build a lookup map: days_exp -> "DD/MM/YYYY"
  # Passed downstream so the Shiny UI shows calendar dates
  # instead of raw day counts in dropdowns and labels.
  expiry_date_map <- final_df %>%
    distinct(days_exp, expiry) %>%
    arrange(days_exp) %>%
    mutate(expiry_label = format(expiry, "%d/%m/%Y")) %>%
    select(days_exp, expiry_label)
  
  cat("Expiry date map:\n")
  print(expiry_date_map)
  
  # Save outputs
  dir.create("data", showWarnings = FALSE)
  write_csv(final_df, "data/options_raw.csv")
  saveRDS(
    list(
      options         = final_df,
      spot            = S0,
      fetch_time      = Sys.time(),
      expiry_date_map = expiry_date_map
    ),
    "data/options_data.rds"
  )
  
  cat("Saved -> data/options_raw.csv\n")
  cat("Saved -> data/options_data.rds\n")
  
  return(list(
    options         = final_df,
    spot            = S0,
    expiry_date_map = expiry_date_map
  ))
}

result <- fetch_spy_options("SPY")
