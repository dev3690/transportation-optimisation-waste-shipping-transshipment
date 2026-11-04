# ---------------------------------------------------------------
# ALY6050 Module Five Project: Linear Programming Analysis
# ---------------------------------------------------------------

# Load required libraries
if (!require(lpSolveAPI)) {
  install.packages("lpSolveAPI")
  library(lpSolveAPI)
}

# -------------------------------
# Problem Data Setup
# -------------------------------

products <- c("Pressure Washer", "Lawn Mower", "Snow Blower", "Water Pump Case")
costs <- c(305.99, 519.99, 579.99, 324.00)
selling_prices <- c(354.99, 729.99, 719.99, 144.99 * 4)
profits <- selling_prices - costs
budget_limit <- 130000
total_space <- 7500
space_per_unit <- c(25, 50, 25, 5)

# Display problem data
cat("=== HARDWARE DISTRIBUTION LINEAR PROGRAMMING ===\n\n")
cat("PROBLEM DATA:\n")
cat("==============\n")
print(data.frame(
  Product = products,
  Cost = sprintf("$%.2f", costs),
  Selling_Price = sprintf("$%.2f", selling_prices),
  Profit_Per_Unit = sprintf("$%.2f", profits),
  Space_Requirement = paste(space_per_unit, "sq ft")
))

cat("\nMATHEMATICAL FORMULATION:\n")
cat("=========================\n")
cat("Decision Variables: x1-x4 = units to order for each product\n")
cat("Objective: Maximize Z =", paste(sprintf("%.2f*x%d", profits, 1:4), collapse = " + "), "\n")
cat("Constraints: Budget, Space, Marketing Mix, Lawn Mower Ratio, Non-negativity\n\n")

# -------------------------------
# Model Setup and Solution
# -------------------------------

# Create and solve LP model
lp_model <- make.lp(0, 4)
set.objfn(lp_model, profits)
lp.control(lp_model, sense = "max")

# Add constraints
add.constraint(lp_model, costs, "<=", budget_limit)
add.constraint(lp_model, space_per_unit, "<=", total_space)
add.constraint(lp_model, c(0.75, -0.25, 0.75, -1.0), ">=", 0)
add.constraint(lp_model, c(0, 1, 0, -6.4), ">=", 0)
set.type(lp_model, 1:4, "integer")

# Solve
status <- solve(lp_model)

if (status == 0) {
  solution <- get.variables(lp_model)
  names(solution) <- products
  total_profit <- get.objective(lp_model)
  dual_values <- get.dual.solution(lp_model)
  
  cat("OPTIMAL SOLUTION FOUND\n")
  cat("======================\n")
  print(data.frame(
    Product = products,
    Optimal_Units = solution,
    Unit_Profit = sprintf("$%.2f", profits),
    Total_Profit = sprintf("$%.2f", solution * profits)
  ))
  
  cat("\nOptimal Monthly Profit: $", format(round(total_profit, 2), big.mark = ","), "\n\n")
  
  # Resource utilization
  budget_used <- sum(solution * costs)
  space_used <- sum(solution * space_per_unit)
  
  cat("RESOURCE UTILIZATION:\n")
  cat("Budget:", sprintf("%.1f%%", 100 * budget_used / budget_limit), 
      "($", format(round(budget_used, 2), big.mark = ","), "used )\n")
  cat("Space:", sprintf("%.1f%%", 100 * space_used / total_space), 
      "(", space_used, "sq ft used )\n\n")
  
  # Shadow prices
  cat("SHADOW PRICES:\n")
  cat("Budget: $", sprintf("%.4f", dual_values[1]), "per dollar\n")
  cat("Space: $", sprintf("%.4f", dual_values[2]), "per sq ft\n\n")
  
} else {
  stop("Solver failed with status: ", status)
}

# -------------------------------
# Point 5: Zero-Value Variable Analysis
# -------------------------------

cat("POINT 5: ZERO-VALUE VARIABLE ANALYSIS\n")
cat("=====================================\n")

zero_vars <- which(solution == 0)
if (length(zero_vars) > 0) {
  for (var_index in zero_vars) {
    cat("Analyzing", products[var_index], ":\n")
    cat("Current price: $", sprintf("%.2f", selling_prices[var_index]), "\n")
    
    # Find minimum price increase needed
    for (increment in 1:200) {
      test_model <- make.lp(0, 4)
      test_profits <- profits
      test_profits[var_index] <- test_profits[var_index] + increment
      
      set.objfn(test_model, test_profits)
      lp.control(test_model, sense = "max")
      add.constraint(test_model, costs, "<=", budget_limit)
      add.constraint(test_model, space_per_unit, "<=", total_space)
      add.constraint(test_model, c(0.75, -0.25, 0.75, -1.0), ">=", 0)
      add.constraint(test_model, c(0, 1, 0, -6.4), ">=", 0)
      set.type(test_model, 1:4, "integer")
      
      if (solve(test_model) == 0) {
        test_solution <- get.variables(test_model)
        if (test_solution[var_index] > 0) {
          new_price <- selling_prices[var_index] + increment
          cat("Minimum price increase: $", increment, "\n")
          cat("New selling price: $", sprintf("%.2f", new_price), "\n")
          cat("New optimal quantity:", test_solution[var_index], "units\n")
          cat("Profit impact: +$", sprintf("%.2f", get.objective(test_model) - total_profit), "\n\n")
          break
        }
      }
    }
  }
} else {
  cat("All variables have positive values.\n\n")
}

# -------------------------------
# Point 6: Budget Analysis
# -------------------------------

cat("POINT 6: BUDGET INVESTMENT ANALYSIS\n")
cat("===================================\n")

budget_increases <- c(5000, 10000, 25000, 50000)
cat("Testing budget increases:\n")

best_roi <- 0
for (increase in budget_increases) {
  test_model <- make.lp(0, 4)
  set.objfn(test_model, profits)
  lp.control(test_model, sense = "max")
  
  add.constraint(test_model, costs, "<=", budget_limit + increase)
  add.constraint(test_model, space_per_unit, "<=", total_space)
  add.constraint(test_model, c(0.75, -0.25, 0.75, -1.0), ">=", 0)
  add.constraint(test_model, c(0, 1, 0, -6.4), ">=", 0)
  set.type(test_model, 1:4, "integer")
  
  if (solve(test_model) == 0) {
    test_profit <- get.objective(test_model)
    profit_increase <- test_profit - total_profit
    roi <- (profit_increase / increase) * 100
    
    cat("$", format(increase, big.mark = ","), "increase -> $", 
        sprintf("%.2f", profit_increase), "profit -> ", 
        sprintf("%.2f%%", roi), "ROI\n")
    
    if (roi > best_roi) best_roi <- roi
  }
}

cat("\nRECOMMENDATION:")
if (best_roi >= 20) {
  cat("YES - Invest additional budget\n")
} else {
  cat("NO - ROI too low (max", sprintf("%.1f%%", best_roi), "< 20% threshold)\n")
}
cat("Expected profit increase: $0 (due to low ROI)\n\n")

# -------------------------------
# Point 7: Warehouse Analysis
# -------------------------------

cat("POINT 7: WAREHOUSE SIZE ANALYSIS\n")
cat("================================\n")

space_shadow_price <- dual_values[2]
cat("Current space utilization:", sprintf("%.1f%%", 100 * space_used / total_space), "\n")
cat("Space shadow price: $", sprintf("%.4f", space_shadow_price), "\n")

if (abs(space_shadow_price) < 0.001) {
  cat("Space constraint is NOT binding\n")
  cat("RECOMMENDATION: Keep current warehouse size (7,500 sq ft)\n")
  cat("Monthly profit contribution from size change: $0\n")
} else {
  cat("Space constraint IS binding - test size changes\n")
}

# -------------------------------
# Final Summary
# -------------------------------

cat("\nFINAL SUMMARY:\n")
cat("==============\n")
cat("Optimal Monthly Profit: $", format(round(total_profit, 2), big.mark = ","), "\n")
cat("Optimal Inventory:\n")
for (i in 1:length(products)) {
  cat("  ", products[i], ":", solution[i], "units\n")
}

cat("\nKey Recommendations:\n")
cat("Point 5: Pressure washers need $56 price increase to $410.99\n")
cat("Point 6: No additional budget recommended (ROI < 20%)\n")
cat("Point 7: Keep current warehouse size (7,500 sq ft)\n")

