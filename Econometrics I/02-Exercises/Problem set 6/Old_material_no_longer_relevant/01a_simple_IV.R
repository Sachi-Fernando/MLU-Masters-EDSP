# Simple Instrumental Variables Laboratory

library(ivreg)
library(ggplot2)
library(AER)
library(stargazer)

set.seed(1234)

### IV in various forms ###
cat("=== SCENARIO 1: OMITTED VARIABLE BIAS ===\n")

# Default parameters
n_obs <- 100
beta_true <- 2
gamma <- .1    # instrument strength (play between 0.01 and 10)
theta <- -1      # exclusion restriction only if zero

# Generate data with omitted variable bias
Z <- rnorm(n_obs)        # Instrument
U <- rnorm(n_obs)        # Error term

# Make instrument (correlated with error if theta not zero)
Z <- 0.7 * Z  + theta*U

# Generate endogenous variable X and outcome Y
X <- gamma * Z   + rnorm(n_obs, 0, 0.5)
Y <- beta_true * X   + U  # O is omitted from regression

# Run models
ols_default <- lm(Y ~ X)
iv_default <- ivreg(Y ~ X | Z)


first_stage_default <- lm(X ~ Z)
second_stage_default <- lm(Y ~ fitted.values(first_stage_default))

cat("Default parameters (gamma = 0.8, rho = 0.6):\n")
cat("True beta:", beta_true, "\n")
cat("OLS estimate:", round(coef(ols_default)["X"], 3), "\n")
cat("IV estimate:", round(coef(iv_default)["X"], 3), "\n")
cat("First stage F-stat:", round(summary(first_stage_default)$fstatistic[1], 2), "\n\n")

stargazer(ols_default,
          iv_default,
          first_stage_default,
          second_stage_default,
          type="text",
          omit.stat = c("F","ser")
          )


# Try weak instrument: gamma = 0.01
gamma_weak <- 0.01
X_weak <- gamma_weak * Z + rnorm(n_obs, 0, 0.5)
Y_weak <- beta_true * X_weak  + U

ols_weak <- lm(Y_weak ~ X_weak)
iv_weak <- ivreg(Y_weak ~ X_weak | Z)
first_stage_weak <- lm(X_weak ~ Z)
second_stage_weak <- lm(Y_weak ~ fitted.values(first_stage_weak))

cat("Weak instrument (gamma = 0.1):\n")
cat("OLS estimate:", round(coef(ols_weak)["X_weak"], 3), "\n")
cat("IV estimate:", round(coef(iv_weak)["X_weak"], 3), "\n")
cat("First stage F-stat:", round(summary(first_stage_weak)$fstatistic[1], 2), "\n\n")


stargazer(ols_weak,
          iv_weak,
          first_stage_weak,
          second_stage_weak,
          type="text",
          omit.stat = c("F","ser"))


