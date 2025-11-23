# Polynomials
rm(list=ls()) 

# Create empty dataset with 100 observations
n <- 100
set.seed(321378) # Set seed for reproducible stochastic results

# Define coefficients
b1 <- 2
b2 <- 1.5 
b3 <- -0.05
b4 <- 0.0005
b5 <- 0.0002

# Generate random uniform x values (between 10 and 100)
z <- runif(n, 10, 100)

# Data generating process (unknown to the researcher)
y4 <- b1 + b2*z + b3*z^2 + b4*z^3 + b5*z^4 + rnorm(n, 0, 200)

# First analysis: polynomial regression up to order 7
max_order1 <- 7
models_list <- list()

r2 <- numeric(max_order1)
ar2 <- numeric(max_order1)
ord <- numeric(max_order1)

for(i in 1:max_order1) {
  # Create polynomial terms manually 
  data_df <- data.frame(y4 = y4)
    for(j in 1:i) {
    data_df[[paste0("z", j)]] <- z^j
  }
  
  # Build formula
  formula <- as.formula(paste("y4 ~", paste(names(data_df)[-1], collapse = " + ")))
  
  # Run polynomial regression
  model <- lm(formula, data = data_df)
  models_list[[i]] <- model
  
  ord[i] <- i
  r2[i] <- summary(model)$r.squared
  ar2[i] <- summary(model)$adj.r.squared
}

# Display model summary using stargazer
stargazer(models_list,
          title = "Polynomial Regression Models (Order 1 to 7)",
          align = TRUE,
          digits = 4,
          dep.var.labels = "y4",
          column.labels = paste("Order", 1:7),
          omit.stat = c("ll", "ser", "f"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          type = "text")


# Create data frame with results
results_df <- data.frame(
  Orden = ord,
  R2 = r2,
  AR2 = ar2
)

# Create plots
library(ggplot2)

# Plot R2 and Adjusted R2 vs Order
gr0 <- ggplot(results_df, aes(x = Orden)) +
  geom_line(aes(y = R2, color = "R²")) +
  geom_line(aes(y = AR2, color = "Adjusted R²")) +
  labs(title = "R² and Adjusted R² vs Polynomial Order",
       x = "Polynomial Order", y = "Value") +
  scale_color_manual(values = c("R²" = "blue", "Adjusted R²" = "red")) +
  theme_minimal()




