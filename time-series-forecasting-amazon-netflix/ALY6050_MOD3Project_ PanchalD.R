# ALY 6050 Module 3 Project: Time Series Forecasting
# Student: Dev Panchal
# Date: 2025-06-10

# =============================================
# 1. INITIAL SETUP
# =============================================


rm(list = ls())

if (!dir.exists("output")) {
  dir.create("output")
  dir.create("output/plots")
}
print_section <- function(title, char = "=") {
  cat("\n", strrep(char, 80), "\n", sep = "")
  cat(toupper(title), "\n")
  cat(strrep(char, 80), "\n\n", sep = "")
}

# ========================================
# 2. LOAD REQUIRED PACKAGES
# ============================================

print_section("Loading Required Packages")

required_packages <- c(
  "forecast", "TTR", "ggplot2", "dplyr", "lmtest", 
  "car", "nortest", "Metrics", "xts", "zoo", 
  "tseries", "fpp2", "gridExtra", "moments"
)

# Install and load packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =============================================
# 3. DATA LOADING AND PREPARATION
# =============================================

print_section("Data Loading and Preparation")


cat("Loading data from ALY6050_MOD3Project_Data_2025SpringB.csv...\n")
stock_data <- read.csv("ALY6050_MOD3Project_Data_2025SpringB.csv")

stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")
amzn_ts <- ts(stock_data$AMZN)
nflx_ts <- ts(stock_data$NFLX)

clean_ts <- function(x) {
  x[!is.finite(x)] <- mean(x, na.rm = TRUE)
  return(ts(x))
}

amzn_ts <- clean_ts(amzn_ts)
nflx_ts <- clean_ts(nflx_ts)

cat("\nData Summary:\n")
print(summary(stock_data[, c("AMZN", "NFLX")]))

# =============================================
# 4. PART 1: SHORT-TERM FORECASTING
# =============================================

print_section("Part 1: Short-term Forecasting")

calculate_mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}
print_section("1. Time Series Plots", "-")

create_ts_plot <- function(ts_data, title, filename) {
  p <- autoplot(ts_data) + 
    ggtitle(title) + 
    ylab("Price ($)") + 
    theme_minimal()
  
  ggsave(file.path("output/plots", filename), 
         plot = p, 
         width = 10, 
         height = 6)
  cat("Plot saved as:", file.path("output/plots", filename), "\n")
  return(p)
}

amzn_plot <- create_ts_plot(amzn_ts, "Amazon (AMZN) Stock Price", "amzn_ts_plot.png")
nflx_plot <- create_ts_plot(nflx_ts, "Netflix (NFLX) Stock Price", "nflx_ts_plot.png")

print_section("2. Exponential Smoothing", "-")

exp_smooth_analysis <- function(ts_data, stock_name, alpha_vals = c(0.20, 0.35, 0.50, 0.65)) {
  cat("\nAnalyzing", stock_name, "...\n")
  results <- data.frame(alpha = numeric(), MAPE = numeric())
  
  for (alpha in alpha_vals) {
    model <- ses(ts_data, alpha = alpha, initial = "simple")
    forecast_vals <- fitted(model)
    mape <- calculate_mape(as.numeric(ts_data), forecast_vals)
    results <- rbind(results, data.frame(alpha = alpha, MAPE = mape))
  }
  
  best_alpha <- results$alpha[which.min(results$MAPE)]
  cat("\nResults for", stock_name, ":\n")
  print(results)
  cat("\nBest alpha:", best_alpha, "with MAPE:", min(results$MAPE), "%\n")
  
  return(list(results = results, best_alpha = best_alpha))
}

amzn_exp <- exp_smooth_analysis(amzn_ts, "AMZN")
nflx_exp <- exp_smooth_analysis(nflx_ts, "NFLX")

print_section("3. Adjusted Exponential Smoothing", "-")

adj_exp_smooth_analysis <- function(ts_data, alpha, stock_name, beta_vals = c(0.20, 0.35, 0.50, 0.65)) {
  cat("\nAnalyzing", stock_name, "with alpha =", alpha, "...\n")
  results <- data.frame(beta = numeric(), MAPE = numeric())
  
  for (beta in beta_vals) {
    model <- holt(ts_data, alpha = alpha, beta = beta, initial = "simple")
    forecast_vals <- fitted(model)
    mape <- calculate_mape(as.numeric(ts_data), forecast_vals)
    results <- rbind(results, data.frame(beta = beta, MAPE = mape))
  }
  
  best_beta <- results$beta[which.min(results$MAPE)]
  cat("\nResults for", stock_name, ":\n")
  print(results)
  cat("\nBest beta:", best_beta, "with MAPE:", min(results$MAPE), "%\n")
  
  return(list(results = results, best_beta = best_beta))
}

amzn_adj <- adj_exp_smooth_analysis(amzn_ts, amzn_exp$best_alpha, "AMZN")
nflx_adj <- adj_exp_smooth_analysis(nflx_ts, nflx_exp$best_alpha, "NFLX")

# =============================================
# 5. PART 2: LONG-TERM FORECASTING
# =============================================

print_section("Part 2: Long-term Forecasting")


print_section("1. 3-Period Weighted Moving Average", "-")

weighted_ma <- function(x, w = c(0.25, 0.30, 0.45)) {
  n <- length(x)
  wma <- rep(NA, n)
  for (i in 4:n) {
    wma[i] <- sum(x[(i-3):(i-1)] * w, na.rm = TRUE)
  }
  return(wma)
}

stock_data$AMZN_WMA <- weighted_ma(stock_data$AMZN)
stock_data$NFLX_WMA <- weighted_ma(stock_data$NFLX)

wma_mape_amzn <- calculate_mape(amzn_ts[4:252], stock_data$AMZN_WMA[4:252])
wma_mape_nflx <- calculate_mape(nflx_ts[4:252], stock_data$NFLX_WMA[4:252])

cat("\nWeighted Moving Average (3-period) MAPE (Periods 4-252):\n")
cat("AMZN:", wma_mape_amzn, "%\n")
cat("NFLX:", wma_mape_nflx, "%\n")

print_section("2. Linear Trend Forecast", "-")

fit_trend <- function(ts_data, start_period = 51, end_period = 252) {
  time <- start_period:end_period
  trend <- lm(ts_data[start_period:end_period] ~ time)
  return(trend)
}

amzn_trend <- fit_trend(amzn_ts)
nflx_trend <- fit_trend(nflx_ts)


forecast_periods <- 51:262

start_date <- as.Date("2020-01-02")
forecast_dates <- seq(start_date, by = "day", length.out = length(forecast_periods))

forecast_df <- data.frame(
  Period = forecast_periods,
  Date = forecast_dates,
  AMZN_Forecast = predict(amzn_trend, newdata = data.frame(time = forecast_periods)),
  NFLX_Forecast = predict(nflx_trend, newdata = data.frame(time = forecast_periods))
)

get_actual_prices <- function() {
  dates <- as.Date(c(
    "2020-12-31", "2021-01-04", "2021-01-05", "2021-01-06", "2021-01-07",
    "2021-01-08", "2021-01-11", "2021-01-12", "2021-01-13", "2021-01-14"
  ))
  
  actual_amzn <- c(
    162.85, 159.33, 160.93, 156.92, 158.11,
    159.13, 155.71, 156.04, 158.29, 156.37
  )

  actual_nflx <- c(
    540.73, 522.86, 520.80, 500.49, 508.89,
    510.40, 499.10, 494.25, 507.79, 500.86
  )
  
  return(data.frame(
    Date = dates,
    AMZN_Actual = actual_amzn,
    NFLX_Actual = actual_nflx
  ))
}

actual_prices <- get_actual_prices()
final_forecast <- tail(forecast_df, 10)

cat("\nComparison of Forecasted vs Actual Prices (Periods 253-262):\n")
comparison <- data.frame(
  Date = actual_prices$Date,
  AMZN_Forecast = final_forecast$AMZN_Forecast,
  AMZN_Actual = actual_prices$AMZN_Actual,
  NFLX_Forecast = final_forecast$NFLX_Forecast,
  NFLX_Actual = actual_prices$NFLX_Actual
)
print(comparison)

final_forecast <- cbind(final_forecast, actual_prices[, c("AMZN_Actual", "NFLX_Actual")])

mape_amzn_253_262 <- calculate_mape(final_forecast$AMZN_Actual, final_forecast$AMZN_Forecast)
mape_nflx_253_262 <- calculate_mape(final_forecast$NFLX_Actual, final_forecast$NFLX_Forecast)

cat("\nLinear Trend Forecast for Periods 253-262:\n")
print(final_forecast)
cat("\nMAPE for Linear Trend Forecast (Periods 253-262):\n")
cat("AMZN:", round(mape_amzn_253_262, 2), "%\n")
cat("NFLX:", round(mape_nflx_253_262, 2), "%\n")

# =============================================
# 6. PART 3: REGRESSION
# =============================================

print_section("Part 3: Regression Analysis")

# (i) Simple regression
print_section("1. Simple Linear Regression", "-")

regression_analysis <- function(ts_data, stock_name) {
  time <- 1:length(ts_data)
  model <- lm(ts_data ~ time)
  forecast_vals <- predict(model, newdata = data.frame(time = 1:262))
  
  # Calculate MAPE for periods 1-252
  mape <- calculate_mape(as.numeric(ts_data), forecast_vals[1:length(ts_data)])
  
  cat("\nRegression Results for", stock_name, ":\n")
  print(summary(model))
  cat("\nMAPE (Periods 1-252):", mape, "%\n")
  
  return(list(model = model, forecast = forecast_vals, mape = mape))
}

# Run regression for both stocks
amzn_reg <- regression_analysis(amzn_ts, "AMZN")
nflx_reg <- regression_analysis(nflx_ts, "NFLX")

# (ii) Residual analysis
print_section("2. Residual Analysis", "-")

analyze_residuals <- function(model, stock_name) {
  res <- residuals(model)
  
  cat("\nResidual Analysis for", stock_name, ":\n")
  
  # Durbin-Watson test
  dw_test <- durbinWatsonTest(model)
  cat("\nDurbin-Watson Test for Autocorrelation:\n")
  print(dw_test)
  
  # Breusch-Pagan test
  bp_test <- bptest(model)
  cat("\nBreusch-Pagan Test for Homoscedasticity:\n")
  print(bp_test)
  
  # Normality tests
  if(length(res) > 5000) {
    cat("\nUsing Anderson-Darling test (sample size > 5000):\n")
    ad_test <- ad.test(res)
    print(ad_test)
  } else {
    shapiro_test <- shapiro.test(res)
    cat("\nShapiro-Wilk Test for Normality:\n")
    print(shapiro_test)
  }
  
  # Q-Q plot
  png(file.path("output/plots", paste0("qq_plot_", tolower(stock_name), ".png")), 
      width = 800, height = 600)
  qqnorm(res, main = paste("Q-Q Plot for", stock_name, "Residuals"))
  qqline(res, col = "red")
  dev.off()
  cat("\nQ-Q plot saved as:", file.path("output/plots", paste0("qq_plot_", tolower(stock_name), ".png")), "\n")
}

analyze_residuals(amzn_reg$model, "AMZN")
analyze_residuals(nflx_reg$model, "NFLX")

# =============================================
# 7. PART 4: BASELINE MODEL AR(3)
# =============================================

print_section("Part 4: Baseline AR(3) Model")

ar3_analysis <- function(ts_data, stock_name) {
  n <- length(ts_data)
  ar3_pred <- rep(NA, n)
  
  for (i in 4:n) {
    ar3_pred[i] <- mean(ts_data[(i-3):(i-1)], na.rm = TRUE)
  }
  
  # Calculate MAPE for periods 4-252
  mape <- calculate_mape(ts_data[4:252], ar3_pred[4:252])
  
  cat("\nAR(3) Model for", stock_name, ":\n")
  cat("MAPE (Periods 4-252):", mape, "%\n")
  
  return(list(forecast = ar3_pred, mape = mape))
}

amzn_ar3 <- ar3_analysis(amzn_ts, "AMZN")
nflx_ar3 <- ar3_analysis(nflx_ts, "NFLX")

# =============================================
# 8. PORTFOLIO ANALYSIS
# =============================================

print_section("Portfolio Analysis")

stock_data$AMZN_Return <- c(NA, diff(log(stock_data$AMZN)))
stock_data$NFLX_Return <- c(NA, diff(log(stock_data$NFLX)))

returns <- na.omit(stock_data[, c("AMZN_Return", "NFLX_Return")])

exp_returns <- colMeans(returns) * 252  # Annualized 
cov_matrix <- cov(returns) * 252        # Annualized covariance

# Portfolio statistics
portfolio_stats <- function(weights) {
  port_return <- sum(weights * exp_returns)
  port_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
  sharpe_ratio <- port_return / port_risk
  return(c(Return = port_return, Risk = port_risk, Sharpe = sharpe_ratio))
}

# Find optimal weights
weight_combinations <- seq(0, 1, by = 0.1)
portfolio_results <- data.frame()

for (w_amzn in weight_combinations) {
  w_nflx <- 1 - w_amzn
  stats <- portfolio_stats(c(w_amzn, w_nflx))
  portfolio_results <- rbind(portfolio_results, 
                             data.frame(Weight_AMZN = w_amzn,
                                        Weight_NFLX = w_nflx,
                                        Return = stats["Return"],
                                        Risk = stats["Risk"],
                                        Sharpe = stats["Sharpe"]))
}

optimal_portfolio <- portfolio_results[which.max(portfolio_results$Sharpe), ]

cat("\nPortfolio Analysis Results:\n")
print(portfolio_results)

cat("\nOptimal Portfolio (Max Sharpe Ratio):\n")
print(optimal_portfolio)

# Create efficient frontier plot
eff_frontier <- portfolio_results[order(portfolio_results$Risk), ]
png("output/plots/efficient_frontier.png", width = 1000, height = 600)
plot(eff_frontier$Risk, eff_frontier$Return, type = "l", col = "blue",
     xlab = "Portfolio Risk (Annualized Volatility)",
     ylab = "Portfolio Return (Annualized)",
     main = "Efficient Frontier: AMZN vs NFLX Portfolio")
points(eff_frontier$Risk, eff_frontier$Return, pch = 19, col = "blue")
points(optimal_portfolio$Risk, optimal_portfolio$Return, 
       pch = 19, col = "red", cex = 1.5)
text(optimal_portfolio$Risk, optimal_portfolio$Return,
     labels = paste0(round(optimal_portfolio$Weight_AMZN*100, 0), "% AMZN\n",
                     round(optimal_portfolio$Weight_NFLX*100, 0), "% NFLX"),
     pos = 4, col = "red")
grid()
legend("topleft", legend = c("Efficient Frontier", "Optimal Portfolio"),
       col = c("blue", "red"), pch = 19, lty = c(1, NA))
dev.off()

cat("\nEfficient frontier plot saved as 'output/plots/efficient_frontier.png'\n")

# =============================================
# 9. SUMMARY OF RESULTS
# =============================================

print_section("Summary of Results")

# summary table of MAPE values
mape_summary <- data.frame(
  Method = c("Exponential Smoothing", "Adjusted Exp. Smoothing", 
             "Weighted MA (3-period)", "Linear Regression", "AR(3)"),
  AMZN_MAPE = c(min(amzn_exp$results$MAPE),
                min(amzn_adj$results$MAPE),
                wma_mape_amzn,
                amzn_reg$mape,
                amzn_ar3$mape),
  NFLX_MAPE = c(min(nflx_exp$results$MAPE),
                min(nflx_adj$results$MAPE),
                wma_mape_nflx,
                nflx_reg$mape,
                nflx_ar3$mape)
)

cat("\nForecasting Accuracy Comparison (MAPE %):\n")
print(mape_summary)

# Best model for each stock
cat("\nBest Performing Models:\n")
cat("AMZN:", mape_summary$Method[which.min(mape_summary$AMZN_MAPE)], 
    "with MAPE =", min(mape_summary$AMZN_MAPE), "%\n")
cat("NFLX:", mape_summary$Method[which.min(mape_summary$NFLX_MAPE)], 
    "with MAPE =", min(mape_summary$NFLX_MAPE), "%\n")

sink("output/results_summary.txt")
cat("ALY 6050 MODULE 3 PROJECT - COMPREHENSIVE RESULTS\n")
cat("Generated on:", date(), "\n\n")
cat("1. Best Performing Models:\n")
cat("   AMZN:", mape_summary$Method[which.min(mape_summary$AMZN_MAPE)], 
    "with MAPE =", min(mape_summary$AMZN_MAPE), "%\n")
cat("   NFLX:", mape_summary$Method[which.min(mape_summary$NFLX_MAPE)], 
    "with MAPE =", min(mape_summary$NFLX_MAPE), "%\n\n")
cat("2. Optimal Portfolio Allocation:\n")
cat("   AMZN:", round(optimal_portfolio$Weight_AMZN * 100, 1), "%\n")
cat("   NFLX:", round(optimal_portfolio$Weight_NFLX * 100, 1), "%\n")
cat("   Expected Annual Return:", round(optimal_portfolio$Return * 100, 2), "%\n")
cat("   Annual Volatility:", round(optimal_portfolio$Risk * 100, 2), "%\n")
cat("   Sharpe Ratio:", round(optimal_portfolio$Sharpe, 2), "\n")
sink()

# Save workspace
save.image("output/ALY6050_Module3_Final_Workspace.RData")


cat("ANALYSIS COMPLETE\n")
