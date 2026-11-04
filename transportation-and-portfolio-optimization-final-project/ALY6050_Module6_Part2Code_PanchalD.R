# ALY6050 Module 6 Final Project - Part 2: Investment Portfolio Optimization
# Risk Minimization Problem
# Name : Dev Panchal
# Install required packages (run once)
# install.packages(c("quadprog", "ggplot2", "dplyr", "knitr"))

library(quadprog)
library(ggplot2)
library(dplyr)

# DATA SETUP


# Asset names
assets <- c("Bonds", "High_tech_stocks", "Foreign_stocks", "Call_options", "Put_options", "Gold")

expected_returns <- c(0.06, 0.22, 0.19, 0.16, 0.15, 0.07)
names(expected_returns) <- assets


covariance_matrix_raw <- matrix(c(
  0.001, -0.003, 0.0027, 0.0009, -0.0006, 0.007,
  -0.003, 0.14, 0.0079, 0.0055, -0.008, 0.004,
  0.0027, 0.0079, 0.15, 0.006, -0.075, -0.003,
  0.0009, 0.0055, 0.006, 0.055, -0.005, -0.006,
  -0.0006, -0.008, -0.075, -0.005, 0.085, -0.004,
  0.007, 0.004, -0.003, -0.006, -0.004, 0.018
), nrow = 6, ncol = 6, byrow = TRUE)

make_positive_definite <- function(mat, min_eigenvalue = 1e-8) {
  mat <- (mat + t(mat)) / 2
  
  eigen_decomp <- eigen(mat)
  eigenvalues <- eigen_decomp$values
  eigenvectors <- eigen_decomp$vectors
  
  eigenvalues[eigenvalues < min_eigenvalue] <- min_eigenvalue
  
  pos_def_matrix <- eigenvectors %*% diag(eigenvalues) %*% t(eigenvectors)
  
  return(pos_def_matrix)
}

covariance_matrix <- make_positive_definite(covariance_matrix_raw)

cat("Original matrix eigenvalues:", eigen(covariance_matrix_raw)$values)
cat("\nAdjusted matrix eigenvalues:", eigen(covariance_matrix)$values)

rownames(covariance_matrix) <- assets
colnames(covariance_matrix) <- assets

investment_amount <- 1000000

cat("=== PORTFOLIO OPTIMIZATION PROBLEM ===")
cat("\nInvestment Amount: $", format(investment_amount, big.mark = ","))
cat("\nAssets and Expected Returns:")
for(i in 1:length(assets)) {
  cat("\n", assets[i], ": ", expected_returns[i] * 100, "%", sep = "")
}

print("\nCovariance Matrix:")
print(covariance_matrix)


# PART 2A: SINGLE OPTIMIZATION (15% Target Return)


cat("\n\n=== PART 2A: MINIMUM RISK FOR 15% TARGET RETURN ===")

# Function to solve portfolio optimization
solve_portfolio <- function(target_return, cov_matrix, exp_returns, budget) {
  n_assets <- length(exp_returns)
  
  max_possible_return <- max(exp_returns)
  min_possible_return <- min(exp_returns)
  
  if(target_return > max_possible_return) {
    cat("Warning: Target return", target_return*100, "% exceeds maximum possible return", max_possible_return*100, "%")
    target_return <- max_possible_return * 0.95 
  }
  
  if(target_return < min_possible_return) {
    cat("Warning: Target return", target_return*100, "% is below minimum possible return", min_possible_return*100, "%")
    target_return <- min_possible_return * 1.05  
  }
  
  # Quadratic programming setup using solve.QP
  # minimize: 0.5 * x' * Dmat * x - dvec' * x
  # subject to: Amat' * x >= bvec
  
  # Dmat: covariance matrix
  Dmat <- 2 * cov_matrix  # multiply by 2 for standard QP form
  
  # dvec: linear term 
  dvec <- rep(0, n_assets)
  
  # Constraint setup for solve.QP format
  # Constraint 1: budget constraint (equality: sum of weights = 1)
  # Constraint 2: return constraint (inequality: return >= target)
  # Constraint 3-n: non-negativity constraints (weights >= 0)
  
  # Amat: constraint matrix 
  Amat <- cbind(
    rep(1, n_assets),          # Budget constraint coefficients
    exp_returns,               # Return constraint coefficients
    diag(n_assets)             # Non-negativity constraint coefficients
  )
  
  # bvec: right-hand side of constraints
  bvec <- c(
    1,                         # Budget = 1 (100%)
    target_return,             # Minimum return
    rep(0, n_assets)           # Non-negativity (>= 0)
  )
  
  # meq: number of equality constraints 
  meq <- 1
  
  # Solve the quadratic program
  tryCatch({
    solution <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
    
    # Extract results
    weights <- solution$solution
    portfolio_return <- sum(weights * exp_returns)
    portfolio_variance <- as.numeric(t(weights) %*% cov_matrix %*% weights)
    portfolio_risk <- sqrt(portfolio_variance)
    
    # Convert to dollar amounts
    dollar_amounts <- weights * budget
    
    return(list(
      weights = weights,
      dollar_amounts = dollar_amounts,
      portfolio_return = portfolio_return,
      portfolio_risk = portfolio_risk,
      portfolio_variance = portfolio_variance,
      success = TRUE
    ))
    
  }, error = function(e) {
    cat("Error in optimization:", e$message, "\n")
    return(list(success = FALSE, error = e$message))
  })
}

# Solve for 15% target return
target_15 <- 0.15
result_15 <- solve_portfolio(target_15, covariance_matrix, expected_returns, investment_amount)

if(result_15$success) {
  cat("\nOptimal Portfolio for 15% Target Return:")
  cat("\nPortfolio Return: ", round(result_15$portfolio_return * 100, 2), "%")
  cat("\nPortfolio Risk (Std Dev): ", round(result_15$portfolio_risk * 100, 2), "%")
  cat("\nPortfolio Variance: ", round(result_15$portfolio_variance, 6))
  
  cat("\n\nOptimal Allocation:")
  allocation_15 <- data.frame(
    Asset = assets,
    Weight = round(result_15$weights, 4),
    Dollar_Amount = round(result_15$dollar_amounts, 2),
    Percentage = round(result_15$weights * 100, 2)
  )
  print(allocation_15)
  
  # Verify constraints
  cat("\nVerification:")
  cat("\nSum of weights: ", round(sum(result_15$weights), 4))
  cat("\nSum of dollar amounts: $", format(round(sum(result_15$dollar_amounts)), big.mark = ","))
  cat("\nAchieved return: ", round(result_15$portfolio_return * 100, 2), "%")
} else {
  cat("\nFailed to solve 15% target return portfolio")
  cat("\nError:", result_15$error)
}


# PART 2B: EFFICIENT FRONTIER ANALYSIS


cat("\n\n=== PART 2B: EFFICIENT FRONTIER ANALYSIS ===")

target_returns <- c(0.08, 0.10, 0.12, 0.14, 0.15, 0.16, 0.18, 0.20)

efficient_frontier <- data.frame(
  Target_Return = numeric(0),
  Portfolio_Return = numeric(0),
  Portfolio_Risk = numeric(0),
  Portfolio_Variance = numeric(0)
)

portfolio_allocations <- list()

cat("\nSolving for multiple target returns...")

for(i in 1:length(target_returns)) {
  target <- target_returns[i]
  cat("\nSolving for target return:", target * 100, "%")
  
  result <- solve_portfolio(target, covariance_matrix, expected_returns, investment_amount)
  
  if(result$success) {
    # Store results
    efficient_frontier <- rbind(efficient_frontier, data.frame(
      Target_Return = target,
      Portfolio_Return = result$portfolio_return,
      Portfolio_Risk = result$portfolio_risk,
      Portfolio_Variance = result$portfolio_variance
    ))
    
    # Store allocation details
    portfolio_allocations[[i]] <- data.frame(
      Target_Return = paste0(target * 100, "%"),
      Asset = assets,
      Weight = result$weights,
      Dollar_Amount = result$dollar_amounts
    )
    
    cat(" - Success! Risk:", round(result$portfolio_risk * 100, 2), "%")
  } else {
    cat(" - Failed:", result$error)
  }
}

# Display efficient frontier results
cat("\n\nEfficient Frontier Results:")
efficient_frontier$Target_Return_Pct <- efficient_frontier$Target_Return * 100
efficient_frontier$Portfolio_Return_Pct <- efficient_frontier$Portfolio_Return * 100
efficient_frontier$Portfolio_Risk_Pct <- efficient_frontier$Portfolio_Risk * 100

print(efficient_frontier[, c("Target_Return_Pct", "Portfolio_Return_Pct", "Portfolio_Risk_Pct")])

# Create efficient frontier plot
cat("\nGenerating Efficient Frontier Plot...")

p1 <- ggplot(efficient_frontier, aes(x = Portfolio_Risk_Pct, y = Portfolio_Return_Pct)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "red", linewidth = 1) +
  labs(
    title = "Efficient Frontier - Risk vs Return",
    x = "Portfolio Risk (Standard Deviation %)",
    y = "Portfolio Return (%)",
    subtitle = "Minimum Risk Portfolios for Different Target Returns"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

print(p1)

cat("\n\n=== MATHEMATICAL RELATIONSHIP ANALYSIS ===")

# Fit different models to understand the relationship
if(nrow(efficient_frontier) > 3) {
  # Linear relationship
  linear_model <- lm(Portfolio_Return ~ Portfolio_Risk, data = efficient_frontier)
  
  # Quadratic relationship
  quadratic_model <- lm(Portfolio_Return ~ Portfolio_Risk + I(Portfolio_Risk^2), data = efficient_frontier)
  
  # Square root relationship
  sqrt_model <- lm(Portfolio_Return ~ sqrt(Portfolio_Risk), data = efficient_frontier)
  
  cat("\nModel Comparisons (R-squared values):")
  cat("\nLinear Model R²: ", round(summary(linear_model)$r.squared, 4))
  cat("\nQuadratic Model R²: ", round(summary(quadratic_model)$r.squared, 4))
  cat("\nSquare Root Model R²: ", round(summary(sqrt_model)$r.squared, 4))
  
  # Correlation analysis
  correlation <- cor(efficient_frontier$Portfolio_Risk, efficient_frontier$Portfolio_Return)
  cat("\nCorrelation coefficient: ", round(correlation, 4))
  
  # Risk-return ratio analysis
  efficient_frontier$Sharpe_Ratio <- (efficient_frontier$Portfolio_Return - 0.06) / efficient_frontier$Portfolio_Risk
  cat("\n\nSharpe Ratios (using 6% risk-free rate):")
  print(round(efficient_frontier[, c("Target_Return_Pct", "Sharpe_Ratio")], 4))
}

# Display detailed allocations for key portfolios
cat("\n\n=== DETAILED PORTFOLIO ALLOCATIONS ===")

key_targets <- c("15%", "12%", "18%")
for(target in key_targets) {
  allocation <- portfolio_allocations[[which(sapply(portfolio_allocations, function(x) x$Target_Return[1]) == target)]]
  if(!is.null(allocation)) {
    cat("\n--- Portfolio for", target, "Target Return ---")
    allocation$Weight_Pct <- round(allocation$Weight * 100, 2)
    allocation$Dollar_Amount <- round(allocation$Dollar_Amount, 0)
    print(allocation[, c("Asset", "Weight_Pct", "Dollar_Amount")])
  }
}

# Summary statistics
cat("\n\n=== SUMMARY STATISTICS ===")
cat("\nMinimum Risk Portfolio:")
min_risk_idx <- which.min(efficient_frontier$Portfolio_Risk)
cat("\n- Target Return: ", efficient_frontier$Target_Return_Pct[min_risk_idx], "%")
cat("\n- Achieved Return: ", round(efficient_frontier$Portfolio_Return_Pct[min_risk_idx], 2), "%")
cat("\n- Risk: ", round(efficient_frontier$Portfolio_Risk_Pct[min_risk_idx], 2), "%")

cat("\n\nMaximum Return Portfolio:")
max_return_idx <- which.max(efficient_frontier$Portfolio_Return)
cat("\n- Target Return: ", efficient_frontier$Target_Return_Pct[max_return_idx], "%")
cat("\n- Achieved Return: ", round(efficient_frontier$Portfolio_Return_Pct[max_return_idx], 2), "%")
cat("\n- Risk: ", round(efficient_frontier$Portfolio_Risk_Pct[max_return_idx], 2), "%")

cat("\n\nRisk Range: ", round(min(efficient_frontier$Portfolio_Risk_Pct), 2), "% to ", 
    round(max(efficient_frontier$Portfolio_Risk_Pct), 2), "%")
cat("\nReturn Range: ", round(min(efficient_frontier$Portfolio_Return_Pct), 2), "% to ", 
    round(max(efficient_frontier$Portfolio_Return_Pct), 2), "%")

# Export results for Word document
#cat("\n\n=== EXPORTING RESULTS ===")
write.csv(efficient_frontier, "efficient_frontier_results.csv", row.names = FALSE)
write.csv(allocation_15, "portfolio_allocation_15pct.csv", row.names = FALSE)

# Save plot
ggsave("efficient_frontier_plot.png", plot = p1, width = 10, height = 6, dpi = 300)



