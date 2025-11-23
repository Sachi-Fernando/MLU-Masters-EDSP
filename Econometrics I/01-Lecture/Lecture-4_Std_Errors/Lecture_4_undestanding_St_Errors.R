
# Set seed for reproducibility
set.seed(5)

# =============================================
# PART 1: GENERATE DATA AND INITIAL PLOTS
# =============================================

# Create dataset with 100 observations
n <- 100
x <- runif(n, 0, 100)  # Uniform distribution between 0-100

# Generate y variables with different error variances
yA <- 40 + 2*x + rnorm(n, mean = 0, sd = 10)  # Low variance
yB <- 40 + 2*x + rnorm(n, mean = 0, sd = 40)  # High variance

# Combine into data frame
data <- data.frame(x, yA, yB)

# Create scatter plot 
par(mar = c(4, 4, 2, 2))  # Adjust margins
plot(x, yA, pch = 19, cex = 0.5, col = "blue",
     xlab = "x", ylab = "y")
points(x, yB, pch = 17, cex = 0.5, col = "red")
legend("topleft", legend = c("yA (low var)", "yB (high var)"),
       col = c("blue", "red"), pch = c(19, 17), cex = 0.7)

# =============================================
# PART 2: REGRESSION TABLES (n=100)
# =============================================

# Run regressions
modelA <- lm(yA ~ x)
modelB <- lm(yB ~ x)

# Compare results
#install.packages("stargazer")
library(stargazer) 
stargazer(modelA, modelB,
          type = "text",
          title = "How the error term's σ affects the estimates",
          column.labels = c("(σ = 10)", "(σ = 40)"),
          dep.var.labels = c("yA", "yB"))

# =============================================
# PART 3: SAMPLING DISTRIBUTION SIMULATION
# =============================================

n_sim <- 1000  # Number of simulations
sample_size <- 30  # Sample size for each simulation

# Initialize vectors to store coefficients
betaA <- numeric(n_sim)
betaB <- numeric(n_sim)

# Simulation loop
for (i in 1:n_sim) {
  # Random sample with replacement
  sample_idx <- sample(n, size = sample_size, replace = TRUE)
  x_sample <- x[sample_idx]
  yA_sample <- yA[sample_idx]
  yB_sample <- yB[sample_idx]
  
  # Run regressions and store coefficients
  betaA[i] <- coef(lm(yA_sample ~ x_sample))[2]
  betaB[i] <- coef(lm(yB_sample ~ x_sample))[2]
}

# =============================================
# PART 4: PLOT SAMPLING DISTRIBUTIONS
# =============================================

# Plot for betaA (low variance case)
hist(betaA, breaks = 50, freq = FALSE, col = "lightblue",
     xlim = c(1.2, 2.6), ylim = c(0, 10),
     main = "Sampling Distribution of β (yA)",
     xlab = "Estimated Coefficient")
curve(dnorm(x, mean = mean(betaA), sd = sd(betaA)), 
      add = TRUE, col = "blue", lwd = 2)
abline(v = 2, col = "red", lty = 2)  # True value

# Plot for betaB (high variance case)
hist(betaB, breaks = 50, freq = FALSE, col = "lightcoral",
     xlim = c(1.2, 2.6), ylim = c(0, 10),
     main = "Sampling Distribution of β (yB)",
     xlab = "Estimated Coefficient")
curve(dnorm(x, mean = mean(betaB), sd = sd(betaB)), 
      add = TRUE, col = "red", lwd = 2)
abline(v = 2, col = "red", lty = 2)  # True value

# =============================================
# PART 5a: SUMMARY STATISTICS
# =============================================

cat("\n\nSimulation Results (n=", n_sim, ")\n", sep = "")
cat("---------------------------\n")
cat("Model A (low variance):\n")
cat("Mean beta:", mean(betaA), "\n")
cat("SD beta:", sd(betaA), "\n\n")
cat("Model B (high variance):\n")
cat("Mean beta:", mean(betaB), "\n")
cat("SD beta:", sd(betaB), "\n")

# =============================================
# PART 5b: CONTRAST WITH REPORTED ESTIMATES
# =============================================

# using the our last n=30 sample:
modelA_n30 <- lm(yA_sample ~ x_sample)
modelB_n30 <- lm(yB_sample ~ x_sample)

stargazer(modelA_n30, modelB_n30,
          type = "text",
          title = "Estimates with n = 30",
          column.labels = c("(σ = 10)", "(σ = 40)"),
          dep.var.labels = c("yA", "yB"))


# St. error estimates are 0.072 (instead of 0.059) in model A
#                     and 0.338 (instead of 0.234) in model B.
