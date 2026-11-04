# ALY6050 Module 6 Final Project - Part 1: Transportation Optimization
# Name : Dev Panchal

library(lpSolve)
rm(list = ls())

# DATA SETUP AND PROBLEM DEFINITION

cat("ALY6050 FINAL PROJECT - PART 1: TRANSPORTATION OPTIMIZATION\n")
cat("============================================================\n\n")

plants <- c("Denver", "Morganton", "Morrisville", "Pineville", "Rockhill", "Statesville")
disposal_sites <- c("Orangeburg", "Florence", "Macon")

supply <- c(40, 33, 42, 55, 27, 43)
names(supply) <- plants

demand <- c(55, 70, 125)
names(demand) <- disposal_sites

cost_matrix <- matrix(c(
  12, 15, 17,  # Denver to [Orangeburg, Florence, Macon]
  11,  7,  8,  # Morganton to [Orangeburg, Florence, Macon]
  11, 18, 13,  # Morrisville to [Orangeburg, Florence, Macon]
  14, 13, 16,  # Pineville to [Orangeburg, Florence, Macon]
  7, 12, 10,  # Rockhill to [Orangeburg, Florence, Macon]
  19, 13, 17   # Statesville to [Orangeburg, Florence, Macon]
), nrow = 6, byrow = TRUE)

rownames(cost_matrix) <- plants
colnames(cost_matrix) <- disposal_sites

plant_to_plant_costs <- matrix(c(
  0,  3,  4,  9,  5,  4,  # Denver
  6,  0,  7,  6,  9,  4,  # Morganton
  5,  7,  0,  3,  4,  9,  # Morrisville
  5,  4,  3,  0,  3, 11,  # Pineville
  5,  9,  5,  3,  0, 14,  # Rockhill
  4,  7, 11, 12,  8,  0   # Statesville
), nrow = 6, byrow = TRUE)

rownames(plant_to_plant_costs) <- plants
colnames(plant_to_plant_costs) <- plants

site_to_site_costs <- matrix(c(
  0, 12, 10,  # Orangeburg
  12,  0, 15,  # Florence
  10, 15,  0   # Macon
), nrow = 3, byrow = TRUE)

rownames(site_to_site_costs) <- disposal_sites
colnames(site_to_site_costs) <- disposal_sites

# Display problem parameters
cat("PROBLEM PARAMETERS:\n")
cat("==================\n")
cat("Plants:", length(plants), "\n")
cat("Disposal Sites:", length(disposal_sites), "\n")
cat("Total Weekly Supply:", sum(supply), "barrels\n")
cat("Total Weekly Capacity:", sum(demand), "barrels\n")
cat("Supply-Demand Balance:", ifelse(sum(supply) <= sum(demand), "Feasible", "Infeasible"), "\n\n")

print("Weekly Waste Generation by Plant:")
print(supply)
cat("\n")

print("Weekly Capacity by Disposal Site:")
print(demand)
cat("\n")

print("Shipping Costs ($ per barrel):")
print(cost_matrix)
cat("\n")


# PART 1A: DIRECT SHIPPING MODEL


cat("PART 1A: DIRECT SHIPPING OPTIMIZATION\n")
cat("==========================================\n\n")

direct_result <- lp.transport(
  cost.mat = cost_matrix,
  direction = "min",
  row.signs = rep("=", length(plants)),
  row.rhs = supply,
  col.signs = rep("<=", length(disposal_sites)),
  col.rhs = demand
)

cat("DIRECT SHIPPING RESULTS:\n")
cat("Status:", direct_result$status, "(0 = optimal solution found)\n")
cat("Optimal Total Cost: $", direct_result$objval, "\n\n")

direct_solution <- direct_result$solution
rownames(direct_solution) <- plants
colnames(direct_solution) <- disposal_sites

cat("Optimal Direct Shipping Plan (barrels per week):\n")
print(round(direct_solution, 1))
cat("\n")

cat("Detailed Direct Shipping Routes:\n")
total_cost_check <- 0
for(i in 1:length(plants)) {
  for(j in 1:length(disposal_sites)) {
    if(direct_solution[i, j] > 0.01) {
      route_cost <- direct_solution[i, j] * cost_matrix[i, j]
      total_cost_check <- total_cost_check + route_cost
      cat(sprintf("%-12s -> %-10s: %5.1f barrels at $%2d/barrel = $%6.2f\n", 
                  plants[i], disposal_sites[j], direct_solution[i, j], 
                  cost_matrix[i, j], route_cost))
    }
  }
}
cat(sprintf("Total Verified Cost: $%.2f\n\n", total_cost_check))

cat("CONSTRAINT VERIFICATION:\n")
cat("Supply Constraints (Generated = Shipped):\n")
for(i in 1:length(plants)) {
  shipped <- sum(direct_solution[i, ])
  cat(sprintf("%-12s: %2d = %5.1f %s\n", plants[i], supply[i], shipped, 
              ifelse(abs(supply[i] - shipped) < 0.01, "✓", "✗")))
}

cat("\nDemand Constraints (Received ≤ Capacity):\n")
for(j in 1:length(disposal_sites)) {
  received <- sum(direct_solution[, j])
  cat(sprintf("%-10s: %5.1f ≤ %3d %s\n", disposal_sites[j], received, demand[j],
              ifelse(received <= demand[j] + 0.01, "✓", "✗")))
}

total_received <- sum(direct_solution)
total_capacity <- sum(demand)
utilization <- (total_received / total_capacity) * 100

cat(sprintf("\nCapacity Utilization: %.1f%% (%d/%d barrels)\n", 
            utilization, as.integer(total_received), total_capacity))

direct_cost <- direct_result$objval
direct_shipping_plan <- direct_solution


# PART 1B: TRANSSHIPMENT MODEL

cat("\n\nPART 1B: TRANSSHIPMENT OPTIMIZATION\n")
cat("===================================\n\n")

#cat("Analyzing potential transshipment improvements...\n\n")

find_transshipment_savings <- function(direct_plan, cost_mat, p2p_costs, s2s_costs) {
  
  total_savings <- 0
  improvements <- list()
  improvement_count <- 0
  
  for(i in 1:nrow(direct_plan)) {
    for(j in 1:ncol(direct_plan)) {
      quantity <- direct_plan[i, j]
      
      if(quantity > 0.01) { 
        direct_cost <- cost_mat[i, j]
        
        for(intermediate_plant in 1:nrow(p2p_costs)) {
          if(intermediate_plant != i) {
            cost_to_intermediate <- p2p_costs[i, intermediate_plant]
            cost_from_intermediate <- cost_mat[intermediate_plant, j]
            total_intermediate_cost <- cost_to_intermediate + cost_from_intermediate
            
            if(total_intermediate_cost < direct_cost) {
              savings_per_barrel <- direct_cost - total_intermediate_cost
              route_savings <- savings_per_barrel * quantity
              total_savings <- total_savings + route_savings
              
              improvement_count <- improvement_count + 1
              improvements[[improvement_count]] <- list(
                type = "plant_intermediate",
                from = plants[i],
                intermediate = plants[intermediate_plant],
                to = disposal_sites[j],
                quantity = quantity,
                direct_cost = direct_cost,
                intermediate_cost = total_intermediate_cost,
                savings = route_savings
              )
            }
          }
        }
        
        current_site_usage <- colSums(direct_plan)
        for(intermediate_site in 1:ncol(direct_plan)) {
          if(intermediate_site != j) {
            available_capacity <- demand[intermediate_site] - current_site_usage[intermediate_site]
            
            if(available_capacity > 0.01) {
              cost_to_intermediate_site <- cost_mat[i, intermediate_site]
              cost_between_sites <- s2s_costs[intermediate_site, j]
              total_site_cost <- cost_to_intermediate_site + cost_between_sites
              
              if(total_site_cost < direct_cost) {
                usable_quantity <- min(quantity, available_capacity)
                savings_per_barrel <- direct_cost - total_site_cost
                route_savings <- savings_per_barrel * usable_quantity
                total_savings <- total_savings + route_savings
                
                improvement_count <- improvement_count + 1
                improvements[[improvement_count]] <- list(
                  type = "site_intermediate",
                  from = plants[i],
                  intermediate = disposal_sites[intermediate_site],
                  to = disposal_sites[j],
                  quantity = usable_quantity,
                  direct_cost = direct_cost,
                  intermediate_cost = total_site_cost,
                  savings = route_savings
                )
              }
            }
          }
        }
      }
    }
  }
  
  return(list(total_savings = total_savings, improvements = improvements))
}

transshipment_analysis <- find_transshipment_savings(
  direct_shipping_plan, cost_matrix, plant_to_plant_costs, site_to_site_costs
)

# Display transshipment results
if(transshipment_analysis$total_savings > 0.01) {
  cat("TRANSSHIPMENT IMPROVEMENTS FOUND:\n")
  
  for(i in 1:length(transshipment_analysis$improvements)) {
    imp <- transshipment_analysis$improvements[[i]]
    
    if(imp$type == "plant_intermediate") {
      cat(sprintf("Route %d: %s → %s → %s\n", i, imp$from, imp$intermediate, imp$to))
    } else {
      cat(sprintf("Route %d: %s → %s → %s\n", i, imp$from, imp$intermediate, imp$to))
    }
    
    cat(sprintf("  Quantity: %.1f barrels\n", imp$quantity))
    cat(sprintf("  Direct cost: $%.2f/barrel vs Transshipment: $%.2f/barrel\n", 
                imp$direct_cost, imp$intermediate_cost))
    cat(sprintf("  Route savings: $%.2f\n\n", imp$savings))
  }
  
  transshipment_cost <- direct_cost - transshipment_analysis$total_savings
  improvement_percentage <- (transshipment_analysis$total_savings / direct_cost) * 100
  
  cat(sprintf("TRANSSHIPMENT SUMMARY:\n"))
  cat(sprintf("Total Transshipment Savings: $%.2f\n", transshipment_analysis$total_savings))
  cat(sprintf("Transshipment Total Cost: $%.2f\n", transshipment_cost))
  cat(sprintf("Cost Improvement: %.2f%%\n", improvement_percentage))
  
} else {
  cat("NO BENEFICIAL TRANSSHIPMENT ROUTES FOUND\n")
  cat("The current direct shipping solution is already optimal.\n")
  cat("No intermediate routing can improve upon the direct shipping costs.\n")
  
  transshipment_cost <- direct_cost
  transshipment_analysis$total_savings <- 0
}

# PART 1C: SENSITIVITY ANALYSIS - CAPACITY EXPANSION


cat("\n\nPART 1C: SENSITIVITY ANALYSIS - CAPACITY EXPANSION\n")
cat("==================================================\n\n")

demand_increased <- demand + 10
names(demand_increased) <- disposal_sites

cat("Capacity Expansion Scenario:\n")
cat("Original Capacities:", paste(demand, collapse = ", "), "\n")
cat("Increased Capacities:", paste(demand_increased, collapse = ", "), "\n")
cat("Additional Capacity:", sum(demand_increased) - sum(demand), "barrels\n\n")

# Solve direct shipping with increased capacity
direct_result_expanded <- lp.transport(
  cost.mat = cost_matrix,
  direction = "min",
  row.signs = rep("=", length(plants)),
  row.rhs = supply,
  col.signs = rep("<=", length(disposal_sites)),
  col.rhs = demand_increased
)

cat("EXPANDED CAPACITY RESULTS:\n")
cat("Direct Shipping (Expanded) Cost: $", direct_result_expanded$objval, "\n")

direct_savings_expansion <- direct_cost - direct_result_expanded$objval
cat("Direct Shipping Savings: $", direct_savings_expansion, "\n")

if(direct_result_expanded$status == 0) {
  expanded_plan <- direct_result_expanded$solution
  rownames(expanded_plan) <- plants
  colnames(expanded_plan) <- disposal_sites
  
  # Recalculate transshipment savings with expanded capacity
  expanded_transshipment <- find_transshipment_savings(
    expanded_plan, cost_matrix, plant_to_plant_costs, site_to_site_costs
  )
  
  transshipment_cost_expanded <- direct_result_expanded$objval - expanded_transshipment$total_savings
  transshipment_savings_expansion <- transshipment_cost - transshipment_cost_expanded
  
  cat("Transshipment (Expanded) Cost: $", round(transshipment_cost_expanded, 2), "\n")
  cat("Transshipment Savings from Expansion: $", round(transshipment_savings_expansion, 2), "\n")
} else {
  transshipment_cost_expanded <- direct_result_expanded$objval
  transshipment_savings_expansion <- direct_savings_expansion
}

# COMPREHENSIVE SUMMARY AND ANALYSIS

cat("\n\nCOMPREHENSIVE RESULTS SUMMARY\n")
cat("===================================\n\n")

results_df <- data.frame(
  Scenario = c(
    "Direct Shipping (Original)",
    "Direct Shipping (Expanded Capacity)",
    "Transshipment (Original)", 
    "Transshipment (Expanded Capacity)"
  ),
  Total_Cost = c(
    direct_cost,
    direct_result_expanded$objval,
    transshipment_cost,
    transshipment_cost_expanded
  ),
  Cost_Savings_vs_Original = c(
    0,
    direct_savings_expansion,
    transshipment_analysis$total_savings,
    direct_cost - transshipment_cost_expanded
  )
)

print(results_df)

# Key insights and recommendations
cat("\n\nKEY INSIGHTS AND STRATEGIC RECOMMENDATIONS:\n")
cat("==================================================\n")

cat("1. BASELINE PERFORMANCE:\n")
cat(sprintf("   - Direct shipping cost: $%.2f\n", direct_cost))
cat(sprintf("   - Capacity utilization: %.1f%%\n", utilization))

cat("\n2. TRANSSHIPMENT ANALYSIS:\n")
if(transshipment_analysis$total_savings > 0.01) {
  improvement_pct <- (transshipment_analysis$total_savings / direct_cost) * 100
  cat(sprintf("   - Potential savings: $%.2f (%.2f%% improvement)\n", 
              transshipment_analysis$total_savings, improvement_pct))
  cat("   - Recommendation: IMPLEMENT transshipment capabilities\n")
  cat("   - Focus on identified intermediate routing opportunities\n")
} else {
  cat("   - No transshipment benefits found\n")
  cat("   - Current network is already optimally configured\n")
  cat("   - Recommendation: MAINTAIN current direct shipping approach\n")
}

cat("\n3. CAPACITY EXPANSION IMPACT:\n")
if(direct_savings_expansion > 0.01) {
  cat(sprintf("   - Direct shipping savings: $%.2f\n", direct_savings_expansion))
  cat(sprintf("   - Cost reduction: %.2f%%\n", (direct_savings_expansion/direct_cost)*100))
} else {
  cat("   - No benefits from capacity expansion\n")
}
cat("   - System is supply-constrained, not demand-constrained\n")
cat("   - Recommendation: Focus on business development over capacity expansion\n")

cat("\n4. STRATEGIC PRIORITIES:\n")
cat("   - Pursue additional waste generation contracts\n")
cat("   - Optimize operational efficiency\n")
if(transshipment_analysis$total_savings > 0.01) {
  cat("   - Invest in intermediate routing capabilities\n")
}
cat("   - Monitor competitors for pricing opportunities\n")

cat("\n5. OPERATIONAL RECOMMENDATIONS:\n")
if(transshipment_analysis$total_savings > 0.01) {
  # Find most frequently used intermediate locations
  intermediate_usage <- table(sapply(transshipment_analysis$improvements, function(x) x$intermediate))
  if(length(intermediate_usage) > 0) {
    top_hub <- names(intermediate_usage)[which.max(intermediate_usage)]
    cat(sprintf("   - Develop %s as primary consolidation hub\n", top_hub))
    cat("   - Invest in enhanced handling capabilities at key locations\n")
  }
}
cat("   - Maintain operational flexibility for demand fluctuations\n")
cat("   - Consider long-term contracts with disposal sites\n")



final_results <- list(
  direct_cost = direct_cost,
  transshipment_cost = transshipment_cost,
  direct_solution = direct_shipping_plan,
  transshipment_savings = transshipment_analysis$total_savings,
  capacity_expansion_savings = direct_savings_expansion,
  summary_table = results_df
)

