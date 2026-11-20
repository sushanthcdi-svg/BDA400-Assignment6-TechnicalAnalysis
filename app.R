# BDA400 Assignment 6
# Technical Analysis Using R - Visualization Phase
# Step 5: Interactive Dashboard Controls

library(shiny)
library(ggplot2)
library(quantmod)

# USER INTERFACE
ui <- fluidPage(
  
  titlePanel("Stock Portfolio Technical Analysis Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Stock Selection"),
      
      selectInput(
        "stock_symbol",
        "Select Stock:",
        choices = c(
          "Apple (AAPL)" = "AAPL",
          "Microsoft (MSFT)" = "MSFT",
          "Google (GOOGL)" = "GOOGL",
          "Tesla (TSLA)" = "TSLA"
        ),
        selected = "AAPL"
      ),
      
      hr(),
      
      h4("Date Range"),
      
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = "2023-01-01",
        end = "2023-07-01"
      ),
      
      hr(),
      
      h4("Time Frame"),
      
      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),
      
      h4("Chart Type"),
      
      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c(
          "Line Chart",
          "Candlestick Chart",
          "Area Chart"
        ),
        selected = "Line Chart"
      ),
      
      h4("Technical Indicators"),
      
      checkboxInput(
        "show_ma",
        "Show Moving Averages (20 & 50)",
        value = TRUE
      ),
      
      checkboxInput(
        "show_rsi",
        "Show RSI (14)",
        value = TRUE
      ),
      
      checkboxInput(
        "show_macd",
        "Show MACD (12, 26, 9)",
        value = TRUE
      ),
      checkboxInput(
        "show_signals",
        "Show Buy/Sell Signals",
        value = TRUE
      ),
      
      hr(),
      
      p("Data Source: Yahoo Finance")
      
    ),
    
    mainPanel(
      
      h3("Stock Price Visualization"),
      
      plotOutput("stock_chart", height = "450px"),
      
      # RSI Indicator Chart
      conditionalPanel(
        condition = "input.show_rsi == true",
        h4("Relative Strength Index (RSI 14)"),
        plotOutput("rsi_chart", height = "250px")
      ),
      
      # MACD Indicator Chart
      conditionalPanel(
        condition = "input.show_macd == true",
        h4("MACD Indicator (12, 26, 9)"),
        plotOutput("macd_chart", height = "280px")
      ),
      
      hr(),
      
      
      hr(),
      
      h3("Trading Signals"),
      
      p("Signals based on SMA20 and SMA50 crossovers."),
      
      tableOutput("signal_table")
      
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Fetch historical stock data based on selected inputs
  stock_data <- reactive({
    
    req(input$stock_symbol, input$date_range)
    
    tryCatch({
      
      getSymbols(
        input$stock_symbol,
        src = "yahoo",
        from = input$date_range[1],
        to = input$date_range[2] + 1,
        auto.assign = FALSE
      )
      
    }, error = function(e) {
      
      NULL
      
    })
    
  })
  
  # Aggregate stock data based on selected time frame
  processed_data <- reactive({
    
    data <- stock_data()
    
    validate(
      need(!is.null(data) && NROW(data) > 0,
           "No stock data available.")
    )
    
    if (input$time_frame == "Weekly") {
      
      data <- to.weekly(data, indexAt = "endof")
      
    } else if (input$time_frame == "Monthly") {
      
      data <- to.monthly(data, indexAt = "endof")
      
    }
    
    # Convert OHLC data into a data frame
    result <- data.frame(
      Date = as.Date(index(data)),
      Open = as.numeric(Op(data)),
      High = as.numeric(Hi(data)),
      Low = as.numeric(Lo(data)),
      Close = as.numeric(Cl(data))
    )
    
    
    result <- result[complete.cases(result), ]
    
    # Calculate Simple Moving Averages
    result$SMA20 <- as.numeric(
      TTR::SMA(result$Close, n = 20)
    )
    
    result$SMA50 <- as.numeric(
      TTR::SMA(result$Close, n = 50)
    )
    
    
    # STEP 8: Calculate Relative Strength Index (RSI)
    result$RSI14 <- as.numeric(
      TTR::RSI(result$Close, n = 14)
    )
    
    # STEP 9: Calculate MACD (12, 26, 9)
    
    if (nrow(result) >= 35) {
      
      macd_values <- TTR::MACD(
        result$Close,
        nFast = 12,
        nSlow = 26,
        nSig = 9,
        maType = "EMA"
      )
      
      result$MACD <- as.numeric(macd_values[, "macd"])
      result$MACD_Signal <- as.numeric(macd_values[, "signal"])
      result$MACD_Histogram <- result$MACD - result$MACD_Signal
      
    } else {
      
      result$MACD <- NA_real_
      result$MACD_Signal <- NA_real_
      result$MACD_Histogram <- NA_real_
      
    }
    
    # STEP 10: Generate Trading Signals
    # Strategy: SMA20 and SMA50 crossover
    
    # Default signal is Hold
    result$Signal <- "Hold"
    
    # Calculate difference between moving averages
    ma_difference <- result$SMA20 - result$SMA50
    
    # Get previous observation's MA difference
    previous_difference <- c(
      NA_real_,
      head(ma_difference, -1)
    )
    
    # Identify bullish crossover
    buy_condition <- (
      previous_difference <= 0 &
        ma_difference > 0
    )
    
    # Identify bearish crossover
    sell_condition <- (
      previous_difference >= 0 &
        ma_difference < 0
    )
    
    # Replace missing logical values with FALSE
    buy_condition[is.na(buy_condition)] <- FALSE
    sell_condition[is.na(sell_condition)] <- FALSE
    
    # Assign signals
    result$Signal[buy_condition] <- "Buy"
    result$Signal[sell_condition] <- "Sell"
    
    # Display a summary in the Console
    print(table(result$Signal))
    
    
    result
    
  })
  
  # Placeholder visualization with data verification
  
  # STEP 6: Interactive Stock Price Visualization
  
  output$stock_chart <- renderPlot({
    
    df <- processed_data()
    
    validate(
      need(nrow(df) > 0, "No stock data available for this period.")
    )
    
    # Create base chart
    p <- ggplot(df, aes(x = Date))
    
    # LINE CHART
    if (input$chart_type == "Line Chart") {
      
      p <- p +
        geom_line(
          aes(y = Close),
          color = "blue",
          linewidth = 1
        )
      
      # AREA CHART
    } else if (input$chart_type == "Area Chart") {
      
      p <- p +
        geom_area(
          aes(y = Close),
          fill = "skyblue",
          alpha = 0.6
        ) +
        geom_line(
          aes(y = Close),
          color = "blue",
          linewidth = 0.8
        )
      
      # CANDLESTICK CHART
    } else if (input$chart_type == "Candlestick Chart") {
      
      df$Direction <- ifelse(
        df$Close >= df$Open,
        "Up",
        "Down"
      )
      
      candle_width <- switch(
        input$time_frame,
        "Daily" = 0.7,
        "Weekly" = 4,
        "Monthly" = 15
      )
      
      p <- ggplot(df, aes(x = Date)) +
        
        geom_linerange(
          aes(ymin = Low, ymax = High),
          color = "black"
        ) +
        
        geom_rect(
          aes(
            xmin = Date - candle_width / 2,
            xmax = Date + candle_width / 2,
            ymin = pmin(Open, Close),
            ymax = pmax(Open, Close),
            fill = Direction
          ),
          color = "black"
        ) +
        
        scale_fill_manual(
          values = c("Up" = "green", "Down" = "red")
        )
      
    }
    
    # STEP 7: Moving Average Overlay
    
    if (isTRUE(input$show_ma)) {
      
      p <- p +
        geom_line(
          data = df,
          aes(x = Date, y = SMA20),
          color = "orange",
          linewidth = 1,
          na.rm = TRUE
        ) +
        geom_line(
          data = df,
          aes(x = Date, y = SMA50),
          color = "purple",
          linewidth = 1,
          na.rm = TRUE
        )
    }
    
    # STEP 11: Buy and Sell Signal Annotations
    
    # Select only crossover events
    buy_signals <- df[
      df$Signal == "Buy" & !is.na(df$Signal),
    ]
    
    sell_signals <- df[
      df$Signal == "Sell" & !is.na(df$Signal),
    ]
    
    # Add Buy markers below the price
    if (isTRUE(input$show_signals) && nrow(buy_signals) > 0) {
      
      p <- p +
        geom_point(
          data = buy_signals,
          aes(x = Date, y = Low),
          color = "darkgreen",
          shape = 24,
          fill = "green",
          size = 3.5,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = buy_signals,
          aes(x = Date, y = Low, label = "BUY"),
          color = "darkgreen",
          vjust = 2,
          size = 3,
          fontface = "bold",
          inherit.aes = FALSE
        )
      
    }
    
    # Add Sell markers above the price
    if (isTRUE(input$show_signals) && nrow(sell_signals) > 0) {
      
      p <- p +
        geom_point(
          data = sell_signals,
          aes(x = Date, y = High),
          color = "darkred",
          shape = 25,
          fill = "red",
          size = 3.5,
          inherit.aes = FALSE
        ) +
        geom_text(
          data = sell_signals,
          aes(x = Date, y = High, label = "SELL"),
          color = "darkred",
          vjust = -1.5,
          size = 3,
          fontface = "bold",
          inherit.aes = FALSE
        )
      
    }
    
    # Labels and theme
    p <- p +
      labs(
        title = paste(
          input$stock_symbol,
          "-",
          input$time_frame,
          input$chart_type
        ),
        x = "Date",
        y = "Stock Price (USD)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 16,
          face = "bold"
        )
      )
    print(p)
  })
  # STEP 8: RSI Visualization
  output$rsi_chart <- renderPlot({
    req(input$show_rsi)
    df <- processed_data()
    validate(
      need(
        any(is.finite(df$RSI14)),
        "Not enough stock data to calculate RSI. Select a longer date range."
      )
    )
    ggplot(df, aes(x = Date, y = RSI14)) +
      geom_line(
        color = "darkorange",
        linewidth = 1
      ) +
      geom_hline(
        yintercept = 70,
        linetype = "dashed",
        color = "red"
      ) +
      geom_hline(
        yintercept = 30,
        linetype = "dashed",
        color = "darkgreen"
      ) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
      ) +
      labs(
        title = paste(input$stock_symbol, "- RSI (14)"),
        x = "Date",
        y = "RSI"
      ) +
      theme_minimal()
  })
  # STEP 9: MACD Visualization
  output$macd_chart <- renderPlot({
    req(input$show_macd)
    df <- processed_data()
    validate(
      need(
        any(is.finite(df$MACD)),
        "Not enough historical data to calculate MACD. Select a longer date range."
      ),
      need(
        any(is.finite(df$MACD_Signal)),
        "Not enough historical data for the MACD signal line."
      )
    )
    # Histogram direction
    df$Histogram_Direction <- ifelse(
      df$MACD_Histogram >= 0,
      "Positive",
      "Negative"
    )
    # Width based on time frame
    histogram_width <- switch(
      input$time_frame,
      "Daily" = 0.8,
      "Weekly" = 4,
      "Monthly" = 18
    )
    ggplot(df, aes(x = Date)) +
      # MACD histogram
      geom_col(
        aes(
          y = MACD_Histogram,
          fill = Histogram_Direction
        ),
        width = histogram_width,
        na.rm = TRUE
      ) +
      # MACD line
      geom_line(
        aes(y = MACD, color = "MACD"),
        linewidth = 1,
        na.rm = TRUE
      ) +
      # Signal line
      geom_line(
        aes(y = MACD_Signal, color = "Signal"),
        linewidth = 1,
        na.rm = TRUE
      ) +
      # Zero reference line
      geom_hline(
        yintercept = 0,
        linetype = "dashed",
        color = "gray50"
      ) +
      scale_fill_manual(
        values = c(
          "Positive" = "forestgreen",
          "Negative" = "firebrick"
        )
      ) +
      scale_color_manual(
        values = c(
          "MACD" = "blue",
          "Signal" = "red"
        )
      ) +
      labs(
        title = paste(input$stock_symbol, "- MACD (12, 26, 9)"),
        x = "Date",
        y = "MACD Value",
        color = "Lines",
        fill = "Histogram"
      ) +
      theme_minimal()
  })
  
  # STEP 10: Display Trading Signals Table
  
  output$signal_table <- renderTable({
    
    df <- processed_data()
    
    validate(
      need(nrow(df) > 0, "No stock data available.")
    )
    
    # Show Date, Closing Price, Moving Averages, and Signal
    signal_table <- data.frame(
      Date = df$Date,
      Close = round(df$Close, 2),
      SMA20 = round(df$SMA20, 2),
      SMA50 = round(df$SMA50, 2),
      Signal = df$Signal
    )
    
    # Display the latest 15 observations
    signal_table <- tail(signal_table, 15)
    
    # Display newest observations first
    signal_table <- signal_table[
      order(signal_table$Date, decreasing = TRUE),
    ]
    
    signal_table
    
  }, striped = TRUE, bordered = TRUE)
  
  }
shinyApp(ui = ui, server = server)