
## Algorithmic Trading Strategy

**Step 1: Data loading**

The AMD stock closing data was loaded into a data frame to form the basis for trading decisions.

```{r}
# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and the Adjusted Close to numeric
amd_df$date <- as.Date(amd_df$Date, format="%d/%m/%Y")
amd_df$close <- as.numeric(amd_df$Adj.Close)

# Select relevant columns
amd_df <- amd_df[, c("date", "close")]

# Implement step 3 to filter the data frame to define the custom trading period
# Define the start and end dates for the custom trading period
start_date <- as.Date("2020/01/01")
end_date <- as.Date("2022/12/31")

# Filter the data frame to the specified data range
amd_df <- subset(amd_df, date >= start_date & date <= end_date)
```

See the plot below to visualise the stock price movement over time.

```{r}
# Plot the price over time
plot(amd_df$date, 
     amd_df$close, 
     type = 'l', 
     main = "AMD stock price over time", 
     xlab = "Date", 
     ylab = "Adjusted close price")
```

**Step 2: Trading algorithm (basic algorithm)**

The trading algorithm was implemented to buy shares if the price of the current day was less than that of the previous day. If the price increased, the trade type was set to 'hold'. Additionally, shares were purchased on the first day of the trading period, and all shares were sold on the last day of the trading period.

```{r}
# Initialise variables for trading logic
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- 0
share_size <- 100 # Define the number of shares to trade each time
n.rows <- nrow(amd_df)

# Implement trading algorithm
for (i in 1:n.rows) {
  # Set up default values for 'hold' trading type
  trade_type <- "hold"
  costs_proceeds <- 0
  accumulated_shares <- amd_df$accumulated_shares[i-1]
  if (i == 1) { 
    # Buy shares on the first day of the trading period
    trade_type <- "buy" 
    costs_proceeds <- -share_size * amd_df$close[i]
    accumulated_shares <- share_size
  } else if (i == n.rows) {
    # Sell all shares on the last day of the trading period
    trade_type <- "sell"
    costs_proceeds <- accumulated_shares * amd_df$close[i]
    accumulated_shares <- 0
  } else if (amd_df$close[i] < amd_df$close[i-1]) {
    # Buy shares if the close price is lower than the previous day
    trade_type <- "buy"
    costs_proceeds <- -share_size * amd_df$close[i]
    accumulated_shares <- accumulated_shares + share_size
  }
  # Update information for each row in data frame
  amd_df$trade_type[i] <- trade_type
  amd_df$costs_proceeds[i] <- costs_proceeds
  amd_df$accumulated_shares[i] <- accumulated_shares
}
```

**Step 4: Run algorithm and analyse results**

The first (basic) algorithm was implemented, executing trades as expected. The total profit / loss and ROI is calculated below.

```{r}
# Calculate total profit
profit <- sum(amd_df$costs_proceeds, na.rm = TRUE) # Sum of costs and proceeds
# Calculate total invested capital
invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
# Calculate ROI
roi <- (profit / invested) * 100

# Print results
print(paste0("Total profit/loss ($AUD): ", round(profit, 2)))
print(paste0("Return on investment (ROI): ", round(roi, 2), "%"))
```

**Step 5: Implement profit taking strategy (profit-taking algorithm)**

A profit taking strategy was implemented to sell half the holdings if the price increased to more than 50% of the average purchase price. Note that if continually executed, the holdings may decrease to a point where there are insufficient shares to sell. If this happens, this algorithm is designed to **block** the buy decision, even if the price decreases from the previous day (assuming the profit threshold is still met), as shares would be purchased at a very high price. The algorithm will wait until the price drops below the profit threshold before purchasing stock again.

```{r}
# Define profit taking parameter
profit_threshold <- 1.5 # Set threshold to 50% above average purchase price

# Initialise average purchase price
amd_df$avg_purchase_price <- 0
amd_df$avg_purchase_price[1] <- amd_df$close[1]

# Implement profit taking strategy
for (i in 2:n.rows) { # Second row used so i-1 can be implemented
  # Set up default values for 'hold' trading type
  avg_purchase <- amd_df$avg_purchase_price[i-1]
  trade_type <- amd_df$trade_type[i]
  shares_to_date <- amd_df$accumulated_shares[i-1]
  accumulated_shares <- shares_to_date
  costs_proceeds <- amd_df$costs_proceeds[i]
  close <- amd_df$close[i]
  if(i == n.rows) {
    # Ensure the profit taking algorithm does not override the sell command on last day
    trade_type <- "sell"
    costs_proceeds <- shares_to_date * close
    accumulated_shares <- 0
  } else if (close > profit_threshold * avg_purchase) {
    # Set up the two cases for when the profit taking threshold is met: when there
    # are sufficient shares to sell half the holdings, and when there are insufficient 
    # shares to sell half the holdings.
    if (accumulated_shares >= 2) {
      # Sell half the holdings if there are sufficient shares to sell
      trade_type <- "sell half"
      # Ensure a whole number of shares are sold
      shares_to_sell <- floor(accumulated_shares / 2) 
      costs_proceeds <- shares_to_sell * close
      # Note tha accumulated shares reflects the shares remaining after the trade 
      # type has been implemented
      accumulated_shares <- accumulated_shares - shares_to_sell
    } else {
      # Block any 'buy' decisions while profit threshold is met
      trade_type <- "hold - insufficient shares to sell"
      costs_proceeds <- 0
      accumulated_shares <- shares_to_date
    }
  } else if (trade_type == "buy") {
    accumulated_shares <- shares_to_date + share_size
    # Update calculation of average purchase price
    avg_purchase <- (avg_purchase*shares_to_date + close*share_size)/accumulated_shares
  } 
  # Update information for each row in data frame
  amd_df$avg_purchase_price[i] <- avg_purchase
  amd_df$trade_type[i] <- trade_type
  amd_df$accumulated_shares[i] <- accumulated_shares
  amd_df$costs_proceeds[i] <- costs_proceeds
} 
```

**Step 6: Summary of findings**

[Profit/Loss (P/L) and Return on Investment (ROI)]{.underline}

The trading algorithms were implemented from 1 January 2020 to 30 December 2022. Two trading algorithms were tested:

1.    **Basic approach**: Buying shares if the share price on any given day was lower than the previous day.

2.    **Profit-taking approach**: Building on the basic approach by additionally selling half of the accumulated shares if the share price was 50% higher than the average purchase price. Note if the share price was 50% higher than the average purchase price by there were no more to sell, the algorithm was designed to block the purchase of additional shares at this high price.

```{r}
# Calculate change to return
# Calculate total profit
new_profit <- sum(amd_df$costs_proceeds)
# Calculate total invested capital
new_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type == "buy"], na.rm = TRUE)
# Calculate ROI
new_roi <- (new_profit / new_invested)*100

# Print results
print(paste0("Profit/loss without profit taking ($AUD): ", round(profit, 0)))
print(paste0("Profit/loss with profit taking ($AUD): ", round(new_profit, 0)))
print(paste0("Return on investment (ROI) without profit taking: ", round(roi, 2), "%"))
print(paste0("Return on investment (ROI) with profit taking: ", round(new_roi, 2), "%"))
```

Results of the first algorithm (basic approach):

·       P/L: -\$791,162

·       ROI: -24.09%

Results of the second algorithm (profit taking approach):

·       P/L: +\$508,002

·       ROI: +17.15%.

Clearly, the second algorithm (profit taking approach) performed better over the customised trading period.

Analysing the buy and sell patterns of the second (profit taking) algorithm compared to the average purchase price from the first (basic) algorithm reveals the significant impact of a profit-taking approach.

```{r}
# Add average purchase price with basic trading algorithm for comparison
# Initialise variable
amd_df$alt_avg_purchase_price <- 0

# Calculate average purchase price (basic algorithm)
for (i in 1:n.rows) {
  alt_price <- 0
  close <- amd_df$close[i]
  trade_type <- amd_df$trade_type[i]
  if (i == 1) {
    # Set average purchase price on the first day to the close price
    alt_price <- close
  } else if (trade_type == "buy") {
    # Calculate average purchasee price (unweighted) for all buy trade type
    alt_price <- mean(amd_df$close[amd_df$trade_type == "buy" & amd_df$date <= amd_df$date[i]])
  } else {
    # Carry forward average purchase price
    alt_price <- amd_df$alt_avg_purchase_price[i-1]
  }
  amd_df$alt_avg_purchase_price[i] <- alt_price
}

# Create plot to compare the two algorithms
# Plot purchase and sell points
# Initialise colour variable
amd_df$colour <- NA

# Set colour to differentiate between trade types
for (i in 1:n.rows) {
  colour <- "white"
  trade_type <- amd_df$trade_type[i]
  if(trade_type == "buy") {
    colour <- "red"
  } else if (trade_type == "sell half") {
    colour <- "blue"
  } else if (trade_type == "sell") {
    colour <- "green"
  }
  amd_df$colour[i] <- colour
}

# Plot the trade type over time
plot(amd_df$date, 
     amd_df$close, 
     col = amd_df$colour, 
     pch = 16, 
     cex = 0.7, 
     main = "AMD trading algorithm comparison", 
     xlab = "Date", 
     ylab = "Adjusted Close Price")
# Add the average purchase price from the basic algorithm over time
lines(amd_df$date, amd_df$alt_avg_purchase_price, col = "grey")
# Add the average purchase price from the profit taking algorithm over time
lines(amd_df$date, amd_df$avg_purchase_price, col = "purple")
# Add the close price over time
lines(amd_df$date, amd_df$close, col = "black")
# Add a legend
legend("topleft",
       c("Close price",
         "Avg purchase price (profit taking)",
         "Avg purchase price (basic approach)", 
         "Buy", 
         "Sell half", 
         "Sell all"), 
       col = c("black", "purple", "grey", "red", "blue", "green"),
       lty = c(1, 1, 1, NA, NA, NA),
       pch = c(NA, NA, NA, 16, 16, 16),
       cex = 0.7,
       pt.bg = c(NA, NA, NA, "red", "blue", "green"))
```

While the profit taking approach in this case resulted in a higher average purchase price (due to the higher weighting of shares purchased after selling events at a high price), it can generate a larger return by generating significant returns by selling at these high prices.

Analysis and Market Event Impact

Notable market events occurred in late 2021, significantly impacting AMD's share price. In late October 2021, AMD was reporting strong earnings, resulting in a 31.7% increased in share price over a single month reflecting investor confidence. Furthermore, in early November 2021, AMD's CEO Lisa Su announced that Meta Platforms had chosen AMD EPYC processors to power its data centres. This announcement generated significant investor optimism about AMD's growth potential, leading to a rapid increase in the share price.

The profit taking algorithm enabled shares to be sold at the peak of this spike, securing high returns. In contrast, the basic algorithm continued to purchase shares at this high price without selling.

Investors were willing to pay a premium for AMD stock due to its promising growth prospects, resulting in a price-earnings ratio of nearly 55 times the year's full earnings estimates. However, as is common in growth sectors, the share price eventually normalised and decreased over 2022. When the trading simulation concluded at the end of 2022, all accumulated shares were sold at a price significantly lower than the average purchase price for both trading algorithms. 

The graph above reflects the higher average purchase price at the end of the trading period when implementing the profit taking algorithm. However, this algorithm still led to profitability as it sold shares at peak prices throughout the trading period, mitigating this loss and retaining profits. This approach also resulted in less holdings for the final sale, meaning there was a smaller loss from selling shares at a lower price from when they were purchased.

In contrast, the first basic algorithm, which only purchased shares until the final trading day, resulted in a loss, as the share price at the end of the simulation was lower than the average purchase price. This comparison reveals the effectiveness of a profit taking algorithm to capture gains during market peaks and highlights the significant impact of market events on share price movements and overall investment returns.

Conclusion

The customised trading period from 1 January 2020 to 30 December 2022 included significant market events that impacted AMD's share price. The profit-taking algorithm effectively capitalised on these events, demonstrating its superiority over this basic approach for this stock over this defined trading period. However, in other cases, the stock may continue to increase beyond the profit threshold, and this profit taking approach could create an opportunity cost by selling all shares prematurely. There are improvement opportunities with this algorithm to identify and follow trends, minimising this risk.

