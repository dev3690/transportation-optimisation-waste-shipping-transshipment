# ALY6050 Module 2 Project: Benefit-Cost Analysis of Dam Construction Projects
# Student: Dev Panchal
# Date: June 3, 2025


# 1. SETUP AND CONFIGURATION



required_pkgs <- c("triangle", "ggplot2", "dplyr", "tidyr", "knitr", "kableExtra")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
lapply(required_pkgs, library, character.only = TRUE)


if (!dir.exists("plots")) dir.create("plots")

# Set seed for reproducibility
set.seed(123)

# Number of simulations
n_sim <- 10000


# 2. DATA DEFINITION


# Define benefits and costs for Dam #1 (Southwest Georgia)
benefits_dam1 <- list(
  B1 = c(1, 3, 4), 
  B2 = c(3, 4, 7), 
  B3 = c(2, 3, 4), 
  B4 = c(3, 4, 5), 
  B5 = c(4, 5, 7), 
  B6 = c(2, 3, 7)  
)

costs_dam1 <- list(
  C1 = c(3, 5, 8), 
  C2 = c(2, 6, 7), 
  C0 = c(3, 4, 6)  
)

# Define benefits and costs for Dam #2 (North Carolina)
benefits_dam2 <- list(
  B1 = c(2, 4, 6), 
  B2 = c(1, 5, 7), 
  B3 = c(2, 3, 4), 
  B4 = c(3, 4, 6), 
  B5 = c(1, 2, 3), 
  B6 = c(3, 4, 5)  
)

costs_dam2 <- list(
  C1 = c(2, 6, 7), 
  C2 = c(1, 5, 6), 
  C0 = c(2, 5, 7)  
)


# 3. SIMULATION FUNCTIONS


simulate_dam <- function(benefits, costs, n = 10000) {
  # Initialize vectors
  n_benefits <- length(benefits)
  n_costs <- length(costs)
  benefit_sims <- matrix(0, nrow = n, ncol = n_benefits)
  cost_sims <- matrix(0, nrow = n, ncol = n_costs)
  
  # Simulate benefits
  for (i in 1:n_benefits) {
    b <- benefits[[i]]
    benefit_sims[, i] <- rtriangle(n, b[1], b[3], b[2])
  }
  
  # Simulate costs
  for (i in 1:n_costs) {
    c <- costs[[i]]
    cost_sims[, i] <- rtriangle(n, c[1], c[3], c[2])
  }
  
  # Calculate totals
  total_benefits <- rowSums(benefit_sims)
  total_costs <- rowSums(cost_sims)
  ratios <- total_benefits / total_costs
  
  return(list(
    benefits = benefit_sims,
    costs = cost_sims,
    total_benefits = total_benefits,
    total_costs = total_costs,
    ratios = ratios
  ))
}


# 4. RUN SIMULATIONS

#cat("Running simulations...\n")
sim_dam1 <- simulate_dam(benefits_dam1, costs_dam1, n_sim)
sim_dam2 <- simulate_dam(benefits_dam2, costs_dam2, n_sim)
#cat("Simulations complete.\n\n")

# 5. ANALYSIS FUNCTIONS

get_summary_stats <- function(ratios, name) {
  stats <- data.frame(
    Statistic = c(
      "Mean", "Median", "Standard Deviation", 
      "Minimum", "Maximum", "P(α > 1)", "P(α > 1.2)", "P(α > 1.5)"
    ),
    Value = c(
      mean(ratios),
      median(ratios),
      sd(ratios),
      min(ratios),
      max(ratios),
      mean(ratios > 1),
      mean(ratios > 1.2),
      mean(ratios > 1.5)
    )
  )
  colnames(stats)[2] <- name
  return(stats)
}

create_freq_table <- function(ratios, dam_name, breaks = 15) {
  brks <- seq(
    floor(min(ratios) * 10) / 10,
    ceiling(max(ratios) * 10) / 10,
    length.out = breaks + 1
  )
  
  freq_table <- data.frame(
    Interval = cut(ratios, breaks = brks, include.lowest = TRUE, dig.lab = 4)
  ) %>%
    count(Interval, name = "Frequency") %>%
    mutate(
      Relative_Frequency = round(Frequency / sum(Frequency), 4),
      Cumulative_Frequency = cumsum(Frequency),
      Cumulative_Relative_Frequency = round(cumsum(Relative_Frequency), 4)
    )
  
  return(freq_table)
}

create_histogram <- function(ratios, dam_name, binwidth = 0.1) {
  df <- data.frame(ratio = ratios)
  p <- ggplot(df, aes(x = ratio)) +
    geom_histogram(aes(y = ..density..), 
                   binwidth = binwidth, 
                   fill = "skyblue", 
                   color = "black") +
    geom_density(alpha = 0.2, fill = "red") +
    geom_vline(aes(xintercept = 1), 
               color = "red", 
               linetype = "dashed",
               linewidth = 1) +
    labs(
      title = paste("Benefit-Cost Ratio Distribution for", dam_name),
      x = "Benefit-Cost Ratio (α)",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  ggsave(paste0("plots/histogram_", gsub("[^[:alnum:]]", "", dam_name), ".png"),
         plot = p, width = 8, height = 5, dpi = 300)
  
  return(p)
}

# 6. ANALYSIS AND VISUALIZATION

#cat("Generating summary statistics...\n")
summary_dam1 <- get_summary_stats(sim_dam1$ratios, "Dam #1")
summary_dam2 <- get_summary_stats(sim_dam2$ratios, "Dam #2")
summary_table <- cbind(summary_dam1, summary_dam2[, -1])

# Display summary table
cat("\n=== SUMMARY STATISTICS ===\n")
print(kable(summary_table, format = "simple", digits = 4))
cat("\n")

#  frequency tables
cat("\n=== FREQUENCY DISTRIBUTION - DAM #1 ===\n")
freq_table_dam1 <- create_freq_table(sim_dam1$ratios, "Dam1")
print(kable(head(freq_table_dam1, 10), format = "simple", digits = 4))
#cat("... [showing first 10 of", nrow(freq_table_dam1), "rows]\n")

cat("\n=== FREQUENCY DISTRIBUTION - DAM #2 ===\n")
freq_table_dam2 <- create_freq_table(sim_dam2$ratios, "Dam2")
print(kable(head(freq_table_dam2, 10), format = "simple", digits = 4))
#cat("... [showing first 10 of", nrow(freq_table_dam2), "rows]\n")

#  histograms
#cat("\nGenerating histograms...\n")
hist_dam1 <- create_histogram(sim_dam1$ratios, "Dam1")
hist_dam2 <- create_histogram(sim_dam2$ratios, "Dam2")

#  Q-Q plots
create_qq_plot <- function(ratios, dam_name) {
  png(paste0("plots/qq_plot_", dam_name, ".png"), 
      width = 800, height = 600)
  qqnorm(ratios, 
         main = paste("Q-Q Plot for", dam_name),
         xlab = "Theoretical Quantiles",
         ylab = "Sample Quantiles")
  qqline(ratios, col = "red")
  dev.off()
}

create_qq_plot(sim_dam1$ratios, "Dam1")
create_qq_plot(sim_dam2$ratios, "Dam2")

#  comparison plot
combined_data <- data.frame(
  Ratio = c(sim_dam1$ratios, sim_dam2$ratios),
  Dam = factor(rep(c("Dam #1", "Dam #2"), each = n_sim))
)

comparison_plot <- ggplot(combined_data, aes(x = Ratio, fill = Dam)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Comparison of Benefit-Cost Ratio Distributions",
    x = "Benefit-Cost Ratio (α)",
    y = "Density",
    fill = "Dam Project"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )

ggsave("plots/comparison_plot.png", 
       plot = comparison_plot, 
       width = 8, height = 5, dpi = 300)

# 7. GOODNESS-OF-FIT TESTING

perform_chi_square_test <- function(ratios, dam_name, breaks = 15) {
  h <- hist(ratios, breaks = breaks, plot = FALSE)
  observed <- h$counts
  breaks <- h$breaks
  
  n <- length(ratios)
  mu <- mean(ratios)
  sigma <- sd(ratios)
  
  expected_probs <- pnorm(breaks[-1], mean = mu, sd = sigma) - 
                   pnorm(breaks[-length(breaks)], mean = mu, sd = sigma)
  
  expected <- n * expected_probs
  while (any(expected < 5)) {
    idx <- which(expected < 5)[1]
    if (is.na(idx)) break
    
    if (idx == 1) {
      expected[idx + 1] <- expected[idx] + expected[idx + 1]
      observed[idx + 1] <- observed[idx] + observed[idx + 1]
      expected <- expected[-(idx)]
      observed <- observed[-(idx)]
      breaks <- breaks[-(idx + 1)]
    } else {
      expected[idx - 1] <- expected[idx - 1] + expected[idx]
      observed[idx - 1] <- observed[idx - 1] + observed[idx]
      expected <- expected[-(idx)]
      observed <- observed[-(idx)]
      breaks <- breaks[-(idx + 1)]
    }
  }
  
  chi_test <- chisq.test(observed, p = expected_probs[1:length(observed)], 
                         rescale.p = TRUE)
  df <- length(observed) - 3
  p_value <- pchisq(chi_test$statistic, df = df, lower.tail = FALSE)
  
  results <- data.frame(
    Test_Statistic = unname(chi_test$statistic),
    Degrees_of_Freedom = df,
    P_Value = p_value
  )
  
  return(results)
}

cat("\n GOODNESS-OF-FIT TESTING \n")
chi_test_dam1 <- perform_chi_square_test(sim_dam1$ratios, "Dam1")
chi_test_dam2 <- perform_chi_square_test(sim_dam2$ratios, "Dam2")

cat("\nDam #1 - Chi-Square Test Results:\n")
print(kable(chi_test_dam1, format = "simple", digits = 4))

cat("\nDam #2 - Chi-Square Test Results:\n")
print(kable(chi_test_dam2, format = "simple", digits = 4))

# 8. PROJECT COMPARISON AND RECOMMENDATION

prob_dam1_gt_dam2 <- mean(sim_dam1$ratios > sim_dam2$ratios)

comparison_metrics <- data.frame(
  Metric = c(
    "Mean Benefit-Cost Ratio",
    "Median Benefit-Cost Ratio",
    "Standard Deviation",
    "Probability(α > 1)",
    "Probability(α > 1.2)",
    "Probability(α > 1.5)"
  ),
  Dam1 = c(
    mean(sim_dam1$ratios),
    median(sim_dam1$ratios),
    sd(sim_dam1$ratios),
    mean(sim_dam1$ratios > 1),
    mean(sim_dam1$ratios > 1.2),
    mean(sim_dam1$ratios > 1.5)
  ),
  Dam2 = c(
    mean(sim_dam2$ratios),
    median(sim_dam2$ratios),
    sd(sim_dam2$ratios),
    mean(sim_dam2$ratios > 1),
    mean(sim_dam2$ratios > 1.2),
    mean(sim_dam2$ratios > 1.5)
  )
)

cat("\n=== PROJECT COMPARISON ===\n")
print(kable(comparison_metrics, format = "simple", digits = 4))

generate_recommendation <- function() {
  dam1_mean <- mean(sim_dam1$ratios)
  dam2_mean <- mean(sim_dam2$ratios)
  
  if (dam1_mean > dam2_mean) {
    recommended <- "Dam #1 (Southwest Georgia)"
    competing <- "Dam #2 (North Carolina)"
    diff <- dam1_mean - dam2_mean
  } else {
    recommended <- "Dam #2 (North Carolina)"
    competing <- "Dam #1 (Southwest Georgia)"
    diff <- dam2_mean - dam1_mean
  }
  
  list(
    recommended = recommended,
    competing = competing,
    mean_diff = diff,
    prob_dam1_gt_dam2 = prob_dam1_gt_dam2
  )
}

rec <- generate_recommendation()

cat("\n=== PROJECT RECOMMENDATION ===\n")
cat("Recommended Project:", rec$recommended, "\n\n")
cat("Key Reasons:\n")
cat("1. Higher mean benefit-cost ratio (", 
    round(ifelse(rec$recommended == "Dam #1 (Southwest Georgia)", 
                 mean(sim_dam1$ratios), 
                 mean(sim_dam2$ratios)), 4), 
    " vs. ", 
    round(ifelse(rec$recommended == "Dam #1 (Southwest Georgia)", 
                 mean(sim_dam2$ratios), 
                 mean(sim_dam1$ratios)), 4), 
    ")\n", sep="")
cat("2. Probability of positive return (α > 1):", 
    round(ifelse(rec$recommended == "Dam #1 (Southwest Georgia)", 
                 mean(sim_dam1$ratios > 1), 
                 mean(sim_dam2$ratios > 1)), 4), "\n")
cat("3. Probability that Dam #1 > Dam #2:", round(prob_dam1_gt_dam2, 4), "\n")


save.image("dam_analysis.RData")
