# Monte Carlo OLS - Raw coefficients (without standardization)
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
    error <- runif(n_obs, -sqrt(12), sqrt(12))  # Uniform with mean 0, variance 4
    y <- true_beta * x + error
    
    # Run OLS regression
    model <- lm(y ~ x )  
    beta_hats[i] <- coef(model)["x"]
  }
  
  # Plot histogram of raw coefficient estimates
  hist(beta_hats, breaks = 30, freq = FALSE, 
       main = paste("Raw Coefficients\nn =", n_obs, "obs/regression"),
       xlab = expression(hat(beta)),
       xlim = c(true_beta - 1, true_beta + 1),
       ylim = c(0, 6),
       col = "lightblue", border = "white")
  
  # Add theoretical sampling distribution
  theoretical_se <- 2 / sqrt(n_obs)  # SD(error)/sqrt(SSX) ≈ 2/sqrt(n_obs)
  curve(dnorm(x, mean = true_beta, sd = theoretical_se), 
        add = TRUE, 
        col = "red", lwd = 2, lty = 2)
  
  # Add vertical line at true parameter
  abline(v = true_beta, col = "darkgreen", lwd = 2)
  
  # Add legend
  legend("topright", 
         legend = c("Theoretical", "True beta"),
         col = c("red", "darkgreen"), lty = c(2, 1), lwd = 2, bty = "n")
}

# Reset plot parameters
par(mfrow = c(1, 1))