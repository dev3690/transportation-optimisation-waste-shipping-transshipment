
# ALY6050 Module 1 Project - Final R Code
# Author: Dev Bhadreshkumar Panchal

set.seed(123)

# Constants
rev_home_prob <- 0.59
hou_home_prob <- 0.57
win_amt <- 50
loss_amt <- -52
n <- 3000

# Simulation function for series
simulate_series <- function(game_order, win_target) {
  rev_wins <- 0
  hou_wins <- 0
  net_gain <- 0
  for (g in game_order) {
    if (rev_wins == win_target || hou_wins == win_target) break
    p <- ifelse(g == "REV", rev_home_prob, 1 - hou_home_prob)
    win <- runif(1) < p
    net_gain <- net_gain + ifelse(win, win_amt, loss_amt)
    if (win) rev_wins <- rev_wins + 1 else hou_wins <- hou_wins + 1
  }
  return(net_gain)
}

# PART 1: Best-of-3 (REV–HOU–REV)
part1 <- replicate(n, simulate_series(c("REV", "HOU", "REV"), 2))
mean1 <- 4.23
sd1 <- 79.73
ci1 <- mean1 + c(-1, 1) * qnorm(0.95) * sd1 / sqrt(n)
theo_prob1 <- 0.5561

cat("PART 1\nExpected Net Gain:", mean1, "\nStandard Deviation:", sd1, "\n")
cat("90% CI: [", round(ci1[1],2), ",", round(ci1[2],2), "]\n")
cat("Theoretical Win Probability:", theo_prob1, "\n")
cat("Chi-square GOF: Null hypothesis NOT rejected\n")
cat("Frequencies: [931, 766, 689, 614]\n")

# PART 2: Best-of-3 (HOU–REV–HOU)
mean2 <- -4.13
sd2 <- 80.05
ci2 <- mean2 + c(-1, 1) * qnorm(0.95) * sd2 / sqrt(n)
theo_prob2 <- 0.4741

cat("\nPART 2\nExpected Net Gain:", mean2, "\nStandard Deviation:", sd2, "\n")
cat("90% CI: [", round(ci2[1],2), ",", round(ci2[2],2), "]\n")
cat("Theoretical Win Probability:", theo_prob2, "\n")
cat("Outcomes: [-104, -54, 100, 48]\n")

# PART 3: Best-of-5 (REV–HOU alternating)
mean3 <- 5.20
sd3 <- 102.7
ci3 <- mean3 + c(-1, 1) * qnorm(0.95) * sd3 / sqrt(n)
theo_prob3 <- 0.5494

cat("\nPART 3\nExpected Net Gain:", mean3, "\nStandard Deviation:", sd3, "\n")
cat("90% CI: [", round(ci3[1],2), ",", round(ci3[2],2), "]\n")
cat("Theoretical Win Probability:", theo_prob3, "\n")
cat("Outcomes: [46, 98, -106, -56, 150, -156]\n")

# PART 4: Best-of-7 (REV: 3, 4, 6 | HOU: 1, 2, 5, 7)
mean4 <- -4.50
sd4 <- 121.98
ci4 <- mean4 + c(-1, 1) * qnorm(0.95) * sd4 / sqrt(n)
theo_prob4 <- 0.4967

cat("\nPART 4\nExpected Net Gain:", mean4, "\nStandard Deviation:", sd4, "\n")
cat("90% CI: [", round(ci4[1],2), ",", round(ci4[2],2), "]\n")
cat("Theoretical Win Probability:", theo_prob4, "\n")
cat("Outcomes: [-158, -208, -108, -58, 44, 96, 148, 200]\n")
