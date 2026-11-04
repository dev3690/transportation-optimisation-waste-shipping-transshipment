
# ALY6050 Module 4 Project - Part I (Inventory Management Decision Model)

cat("\n\nPart I : Inventory Management Decision Model\n")

# 1. Define data and parameters
annual_demand <- 12000  # Annual demand in units
unit_cost <- 159.99     # Cost per unit
holding_rate <- 0.115   # Holding cost rate (11.5%)
order_cost <- 179.99    # Cost per order
policy_multiplier <- 1.8

# 2. Define mathematical functions
calculate_ordering_cost <- function(Q) {
  annual_orders <- annual_demand / Q
  return(annual_orders * order_cost)
}

calculate_holding_cost <- function(Q) {
  avg_inventory <- Q / 2
  holding_cost_per_unit <- unit_cost * holding_rate
  return(avg_inventory * holding_cost_per_unit)
}

calculate_total_cost <- function(Q) {
  ordering_cost <- calculate_ordering_cost(Q)
  holding_cost <- calculate_holding_cost(Q)
  return(ordering_cost + holding_cost)
}

# 3. Analyze order quantities
Q_values <- seq(100, 3000, by = 10)
total_costs <- sapply(Q_values, calculate_total_cost)

optimal_index <- which.min(total_costs)
optimal_Q <- Q_values[optimal_index]
minimum_total_cost <- total_costs[optimal_index]

cat("\nApproximate Optimal Order Quantity:", optimal_Q)
cat("\nMinimum Total Cost: $", round(minimum_total_cost, 2))

# 4. EOQ calculation
EOQ <- sqrt((2 * annual_demand * order_cost) / (unit_cost * holding_rate))
EOQ_cost <- calculate_total_cost(EOQ)
cat("\nAnalytical Optimal Order Quantity (EOQ):", round(EOQ))
cat("\nAnalytical Minimum Total Cost: $", round(EOQ_cost, 2))

# 5. Company policy Q
policy_Q <- EOQ * policy_multiplier
policy_cost <- calculate_total_cost(policy_Q)
cat("\nCompany Policy Order Quantity (1.8x EOQ):", round(policy_Q))
cat("\nTotal Cost using Policy Q: $", round(policy_cost, 2))

# 6. Plot total cost vs. order quantity
png("total_cost_plot.png", width = 800, height = 600)
plot(Q_values, total_costs, type = "l", col = "blue",
     xlab = "Order Quantity", ylab = "Total Annual Cost ($)",
     main = "Total Cost vs. Order Quantity")
points(optimal_Q, minimum_total_cost, col = "red", pch = 16)
text(optimal_Q, minimum_total_cost,
     labels = paste("Optimal Q =", round(optimal_Q)),
     pos = 3)
dev.off()

# 7. Sensitivity analysis
holding_rates <- seq(0.08, 0.115, by = 0.001)
order_costs_grid <- seq(150, 210, by = 10)
sensitivity_matrix <- matrix(NA, nrow = length(holding_rates), ncol = length(order_costs_grid))

for (i in 1:length(holding_rates)) {
  for (j in 1:length(order_costs_grid)) {
    current_hr <- holding_rates[i]
    current_oc <- order_costs_grid[j]
    EOQ_sens <- sqrt((2 * annual_demand * current_oc) / (unit_cost * current_hr))
    ordering_cost <- (annual_demand / EOQ_sens) * current_oc
    holding_cost <- (EOQ_sens / 2) * unit_cost * current_hr
    sensitivity_matrix[i, j] <- ordering_cost + holding_cost
  }
}

png("sensitivity_analysis.png", width = 800, height = 600)
image(holding_rates, order_costs_grid, sensitivity_matrix,
      col = heat.colors(100), xlab = "Holding Rate",
      ylab = "Order Cost", main = "Sensitivity Analysis: Total Cost")
contour(holding_rates, order_costs_grid, sensitivity_matrix, add = TRUE)
dev.off()

cat("\n\nSummary Statistics:")
cat("\n-------------------")
cat("\nAnnual Demand:", annual_demand, "units")
cat("\nUnit Cost: $", unit_cost)
cat("\nHolding Rate:", holding_rate * 100, "%")
cat("\nOrder Cost: $", order_cost)
cat("\nOptimal Order Quantity:", round(optimal_Q), "units")
cat("\nOptimal Number of Orders per Year:", round(annual_demand / optimal_Q, 1))
cat("\nOptimal Order Interval:", round(365 / (annual_demand / optimal_Q), 1), "days")
cat("\nMinimum Total Annual Cost: $", round(minimum_total_cost, 2))


# ===================
# Part II: Simulation
# ===================

cat("\n\n\nPart II: Inventory Management Simulation with Triangular Distribution")

set.seed(80311)
library(fitdistrplus)

# Simulation configuration
n_sim <- 3000
min_demand <- 8000
max_demand <- 15000
mode_demand <- 12000
order_cost <- 210

# Triangular random generator
rtriangular <- function(n, min, max, mode) {
  u <- runif(n)
  f <- (mode - min) / (max - min)
  ifelse(u < f,
         min + sqrt(u * (max - min) * (mode - min)),
         max - sqrt((1 - u) * (max - min) * (max - mode)))
}

# Cost functions
calculate_total_cost_sim <- function(D, Q) {
  ordering_cost <- (D / Q) * order_cost
  holding_cost <- (Q / 2) * unit_cost * holding_rate
  return(ordering_cost + holding_cost)
}

find_optimal_Q <- function(D) {
  return(sqrt((2 * D * order_cost) / (unit_cost * holding_rate)))
}

# Run simulation
sim_results <- data.frame(demand = rtriangular(n_sim, min_demand, max_demand, mode_demand))
sim_results$optimal_Q <- sapply(sim_results$demand, find_optimal_Q)
sim_results$min_total_cost <- mapply(calculate_total_cost_sim, sim_results$demand, sim_results$optimal_Q)
sim_results$annual_orders <- sim_results$demand / sim_results$optimal_Q

# Confidence intervals
calc_ci <- function(x) {
  m <- mean(x)
  se <- sd(x) / sqrt(length(x))
  ci <- m + qt(c(0.025, 0.975), df=length(x)-1) * se
  return(list(mean=m, lower=ci[1], upper=ci[2]))
}

cost_ci <- calc_ci(sim_results$min_total_cost)
q_ci <- calc_ci(sim_results$optimal_Q)
orders_ci <- calc_ci(sim_results$annual_orders)

cat("\nSimulation Results (95% Confidence Intervals):")
cat("\nMinimum Total Cost: Mean =", round(cost_ci$mean, 2), 
    ", 95% CI = [", round(cost_ci$lower, 2), ",", round(cost_ci$upper, 2), "]")
cat("\nOptimal Order Quantity: Mean =", round(q_ci$mean, 2), 
    ", 95% CI = [", round(q_ci$lower, 2), ",", round(q_ci$upper, 2), "]")
cat("\nAnnual Orders: Mean =", round(orders_ci$mean, 2), 
    ", 95% CI = [", round(orders_ci$lower, 2), ",", round(orders_ci$upper, 2), "]")

# Distribution fitting
cat("\n\nFitting Distributions:")
fit_norm <- fitdist(sim_results$min_total_cost, "norm")
fit_lnorm <- fitdist(sim_results$min_total_cost, "lnorm")
cat("\nNormal AIC:", fit_norm$aic, ", Lognormal AIC:", fit_lnorm$aic)

# Histogram with density
# Save simulation distribution plots to PNG
png("simulation_distributions.png", width = 800, height = 600)
par(mfrow=c(2,2))

hist(sim_results$min_total_cost, breaks=30, prob=TRUE, main="Min Total Cost", xlab="Cost")
lines(density(sim_results$min_total_cost), col="red")

hist(sim_results$optimal_Q, breaks=30, prob=TRUE, main="Optimal Q", xlab="Q")
lines(density(sim_results$optimal_Q), col="red")

hist(sim_results$annual_orders, breaks=30, prob=TRUE, main="Annual Orders", xlab="Orders")
lines(density(sim_results$annual_orders), col="red")

hist(sim_results$demand, breaks=30, prob=TRUE, main="Demand Distribution", xlab="Demand")
lines(density(sim_results$demand), col="red")

dev.off()

# Shapiro tests
cat("\n\nNormality Tests:")
cat("\nMin Total Cost:")
print(shapiro.test(sim_results$min_total_cost))
cat("\nOptimal Q:")
print(shapiro.test(sim_results$optimal_Q))
cat("\nAnnual Orders:")
print(shapiro.test(sim_results$annual_orders))
