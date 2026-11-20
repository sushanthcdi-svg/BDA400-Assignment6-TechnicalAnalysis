
# BDA400 Assignment 6
# Step 3: Historical Stock Data Collection

# Load required package
library(quantmod)

# Define stock symbol and date range
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

# Download historical data from Yahoo Finance
stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)

# Display first six rows
head(stock_data)

# Display last six rows
tail(stock_data)

# Check total number of observations
nrow(stock_data)