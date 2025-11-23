# Monte Carlo OLS - Root-n adjusted
set.seed(123)

# True parameters
true_beta <- 2
n_sim <- 1000  # Number of Monte Carlo simulations
sample_sizes <- c(10, 50, 200, 1000)  # Observations per regression

# Create plot layout
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (n_obs in sample_sizes) {
  # Store OLS estimates
  beta_hats <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    # Generate data: y = 2*x + error
    x <- rnorm(n_obs)
    #error <- rnorm(n_obs, sd = 2)
    error <- runif(n_obs, -sqrt(12), sqrt(12))  # Uniform with mean 0, variance 4
    y <- true_beta * x + error
    
    # Run OLS regression
    model <- lm(y ~ x )
    beta_hats[i] <- coef(model)["x"]
  }
  
  # PROPER standardization for root-n consistency
  # Finite-sample variance: Var(beta_hat) = σ²/(n * E[x²]) = 4/n_obs
  # Asymptotic variance: V = σ²/E[x²] = 4
  # Root-n scaling: sqrt(n_obs)(beta_hat - beta) ~ N(0, V)
  adjusted_betas <- sqrt(n_obs) * (beta_hats - true_beta) #/ 2
  
  # Plot histogram of adjusted coefficients
  hist(adjusted_betas, breaks = 30, freq = FALSE, 
       main = paste("Root-n adjusted\nn =", n_obs, "obs/regression"),
       xlab = expression(sqrt(n)*(hat(beta) - beta)),
       xlim = c(-8, 8), ylim = c(0, 0.2),
       col = "lightblue", border = "white")
  
  # Add standard normal density curve
  curve(dnorm(x,0,2), from = -4, to = 4, add = TRUE, 
        col = "red", lwd = 2, lty = 2)
  
  # Add vertical line at zero
  abline(v = 0, col = "darkgreen", lwd = 2)
  
  # Add legend
  legend("topright", 
         legend = c("N(0,4)", "Zero"),
         col = c("red", "darkgreen"), lty = c(2, 1), lwd = 2, bty = "n")
}

# Reset plot parameters
par(mfrow = c(1, 1))