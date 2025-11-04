# Bias-efficiency trade off Laboratory

# Clear and set replication seed 
  rm(list=ls()) 
  set.seed(123) # optional, for exact reproducibility


# Parameters to play with
n <- 50         # Number of observations
sigma2 <- 1     # Error variance
beta1 <- 2      # True coefficient for x1
beta2 <- -2     # True coefficient for x2 (set to 0 for irrelevant variable)
delta <- 0.9    # Relationship between x1 and x2 (cov(x1,x2)/var(x1))


# Generate data
x1 <- rnorm(n)
x2 <- delta*x1 + rnorm(n, sd = sqrt(1 - delta^2))  # Ensure var(x2)=1
u <- rnorm(n, sd = sqrt(sigma2))
y <- beta1*x1 + beta2*x2 + u
irrelevant <- 1.2*x1 + .1*rnorm(n) # irrelevant but correlated with x1

# Estimate models
short_model <- lm(y ~ x1)          # Omits x2
true_model <- lm(y ~ x1 + x2)      # Correct specification
overfit_model <- lm(y ~ x1 + x2 + irrelevant)  # Includes irrelevant variable

# Display results
library(stargazer)
stargazer(short_model, true_model, overfit_model,
          type = "text",
          title = "Model Comparison Results",
          column.labels = c("Omitted", "Correct", "Overfit"),
          covariate.labels = c("x1", "x2", "Irrelevant"),
          keep.stat = c("n", "rsq", "ser"))

# Calculate theoretical bias
theoretical_bias <- beta2 * delta
cat(sprintf("\nTheoretical bias when omitting x2: %.3f\n", theoretical_bias))
cat(sprintf("Actual bias in short model: %.3f\n", coef(short_model)["x1"] - beta1))

# Calculate VIF
vif_x1 <- 1/(1 - summary(lm(x1 ~ x2))$r.squared)
cat(sprintf("\nVariance Inflation Factor (VIF) for x1: %.3f", vif_x1))