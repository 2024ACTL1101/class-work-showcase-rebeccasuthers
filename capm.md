# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

```{r load-data}
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", 
                       from = start_date, 
                       to = end_date, 
                       auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", 
                        from = start_date, 
                        to = end_date, 
                        auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", 
                      from = start_date, 
                      to = end_date, 
                      auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```{r data}
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

\[ E(R_i) = R_f + \beta_i (E(R_m) - R_f) \]

Where:

- \( E(R_i) \) is the expected return on the capital asset,
- \( R_f \) is the risk-free rate,
- \( \beta_i \) is the beta of the security, which represents the systematic risk of the security,
- \( E(R_m) \) is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```{r return}
# Initialise variables
df$AMD_return <- 0
df$GSPC_return <- 0

# Calculate returns
for (i in 2:nrow(df)) {
  df$AMD_return[i] <- (df$AMD[i] - df$AMD[i-1])/ df$AMD[i-1]
  df$GSPC_return[i] <- (df$GSPC[i] - df$GSPC[i-1])/ df$GSPC[i-1]
}
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```{r riskfree}
for (i in 1:nrow(df)) {
  df$RF_daily[i] <- (1 + df$RF[i]/100)^(1/360) - 1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```{r excess return}
# Initialise variables
df$AMD_excess <- 0
df$GSPC_excess <- 0

# Calculate excess return
for (i in 2:nrow(df)) {
  df$AMD_excess[i] <- df$AMD_return[i] - df$RF_daily[i]
  df$GSPC_excess[i] <- df$GSPC_return[i] - df$RF_daily[i]
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```{r lm}
# Remove first row of dataframe from linear model
df <- df[-1, ]

# Create linear model
capm_model <- lm(AMD_excess ~ GSPC_excess, data = df)
summary(capm_model)
```

#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:** The beta value indicates how volatile AMD is in comparison to the market. A beta 
greater than 1 indicates that AMD is more volatile than the market (GSPC), whereas a beta of less
than 1 indicates that AMD is less volatile than the market.

The beta value of 1.57 (2 d.p.) indicates that AMD is more volatile than the market. Beta 
measures the systematic risk of a stock, and a beta value of 1.57 indicates that AMD is 
more sensitive to market movements compared to the typical stock, which has a beta of 1. This
higher sensitivity means that AMD is more affected by market-wide events. 

Given that AMD has a higher beta, investors can expect higher potential returns during market
highs, and greater losses during market lows. This makes AMD a riskier investment compared 
to the overall market. Investors looking to diversify their portfolios might include AMD if 
they seek higher returns and are willing to accept higher risk - but they must ensure the balance 
their portfolio with lower-beta stocks to mitigate overall risk.


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```{r plot}
ggplot(data = df, mapping = aes(x = GSPC_excess, y = AMD_excess)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "CAPM Analysis: AMD vs S&P 500 Excess Returns",
       x = "S&P 500 Excess Return",
       y = "AMD Excess Return")
```
### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.

*Hint: Calculate the daily standard error of the forecast ($s_f$), and assume that the annual standard error for prediction is $s_f \times \sqrt{252}$. Use the simple return average method to convert daily stock returns to annual returns if needed.*


**Answer:**

```{r pi}
# Calculate expected annual return
beta <- coef(capm_model)[2]
rf <- 0.05
GSPC <- 0.133
GSPC_excess <- GSPC - rf
expected_return <- rf + beta*(GSPC - rf)
n <- length(df$GSPC)

# Calculate prediction interval
mean_GSPC <- mean(df$GSPC_excess)
SE <- summary(capm_model)$sigma
SSX <- sum((df$GSPC_excess - mean_GSPC)^2)
sf <- SE * sqrt(1 + 1/n + (GSPC_excess - mean_GSPC)^2 / SSX) * sqrt(252)

alpha <- 0.10
t_value <- qt(1 - alpha/2, df = n - 2)
lower_bound <- expected_return - t_value * sf
upper_bound <- expected_return + t_value * sf

print(paste0("Lower bound: ", round(lower_bound*100, 2), "%, ",
             "Expected Return: ", round(expected_return*100, 2), "%, ",
             "Upper Bound: ", round(upper_bound*100, 2), "%"))
```

The Capital Asset Pricing Model (CAPM) was applied to analyse AMD's stock performance 
relative to the S&P 500. Using the CAPM, we estimated the expected return for AMD
and determined a 90% prediction interval for AMD's annual expected return. Given a 
current risk-free rate of 5.0% and an annual expected return for the S&P 500 of 13.3%,
the prediction interval for AMD's annual expected return ranges from -50.08% to 86.14%, 
with an expected return of 18.03%.

This wide prediction interval reflects the high volatility and systematic risk associated 
with AMD's stock, emphasising the potential for both substantial gains and significant losses.
Investors should consider this volatility when making investment decisions and managing their 
portfolios to balance risk and return.
