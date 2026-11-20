
# BDA400 Assignment 6
## Technical Analysis Using R — Visualization Phase

### Project Description
This project implements an interactive stock portfolio
dashboard using R Shiny.

The dashboard retrieves historical stock data from
Yahoo Finance and provides interactive visualizations
and technical analysis tools.

### Features
- Historical stock data collection
- Stock selection (AAPL, MSFT, GOOGL, TSLA)
- Custom date-range selection
- Daily, Weekly, and Monthly time frames
- Line, Candlestick, and Area charts
- 20-period and 50-period Simple Moving Averages
- Relative Strength Index (RSI 14)
- MACD (12, 26, 9)
- Buy, Sell, and Hold trading signals
- Chart annotations for moving-average crossovers
- Interactive indicator controls

### Technologies Used
- R
- Shiny
- ggplot2
- quantmod
- TTR
- Yahoo Finance

### Project Files
- app.R — Interactive dashboard application
- stock_data.R — Historical stock data collection

### How to Run
1. Open the project in RStudio or Posit Cloud.
2. Install the required packages:

   install.packages(c("shiny", "ggplot2",
                     "quantmod", "TTR"))

3. Open app.R.
4. Click Run App.

### Trading Strategy
Buy: SMA20 crosses above SMA50.

Sell: SMA20 crosses below SMA50.

Hold: No new moving-average crossover occurs.

### Academic Purpose
This project was developed for BDA400
Data Science Tools and Techniques,
Assignment 6 — Visualization Phase.

Trading signals are educational demonstrations,
not financial advice.
