# Load required packages
library(ggplot2)
library(AER)
library(foreign)

# Set seed for reproducibility
set.seed(123)

# Generate data
n_obs <- 100

# True parameters
a_c <- 1000  # Crime intercept
b_c <- -2    # Effect of police on crime
g_c <- 10    # Effect of unemployment on crime

a_p <- 10    # Police intercept  
b_p <- 1.5   # Effect of crime on police
g_p <- 1     # Effect of budget on police

# Generate exogenous variables
Unemp <- runif(n_obs, 0, 10)        # unemployment measure
RandBudg <- rnorm(n_obs, 100, 10)     # Budget (random)

# Generate error terms
e_p <- rnorm(n_obs, 0, 1)
e_c <- rnorm(n_obs, 0, 1)

# Generate simultaneous equations system
# Reduced form for Crime
Crime <- (a_c + b_c*(a_p + g_p*RandBudg + e_p) + g_c*Unemp + e_c)/(1 - b_c*b_p)

# Structural equation for Police
Polic <- a_p + b_p*Crime + g_p*RandBudg + e_p


# Create data frame
crime_data <- data.frame(
  Crime = Crime,
  Polic = Polic,
  Unemp = Unemp,
  RandBudg = RandBudg
)

# Plot relationship between Crime and Police
g1 <- ggplot(crime_data, aes(x = Polic, y = Crime)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between Crime and Police",
       x = "Police Presence",
       y = "Crime Rate") +
  theme_minimal()

print(g1)



######################################################################
# We want to estimate the effect of Police on Crime. How do we do it?#
######################################################################

cat("\n=== ESTIMATING EFFECT OF POLICE ON CRIME ===\n")

# Two-stage least squares manual approach
# First stage: Police ~ RandBudg + Unemp
first_stage_police <- lm(Polic ~ RandBudg + Unemp, data = crime_data)
crime_data$Polic_hat <- predict(first_stage_police)

# Second stage: Crime ~ Police_hat + Unemp  
second_stage_crime <- lm(Crime ~ Polic_hat + Unemp, data = crime_data)
cat("Manual 2SLS (Police effect on Crime):", coef(second_stage_crime)["Polic_hat"], "\n")

# Direct 2SLS using AER package
tsls_crime <- ivreg(Crime ~ Polic + Unemp | Unemp + RandBudg, data = crime_data)
cat("Direct 2SLS (Police effect on Crime):", coef(tsls_crime)["Polic"], "\n")

######################################################################
# We want to estimate the effect of Crime on Police. How do we do it?#
######################################################################

cat("\n=== ESTIMATING EFFECT OF CRIME ON POLICE ===\n")



# Two-stage least squares manual approach
# First stage: Crime ~ RandBudg + Unemp
first_stage_crime <- lm(Crime ~ RandBudg + Unemp, data = crime_data)
crime_data$Crime_hat <- predict(first_stage_crime)

# Second stage: Police ~ Crime_hat + RandBudg
second_stage_police <- lm(Polic ~ Crime_hat + RandBudg, data = crime_data)
cat("Manual 2SLS (Crime effect on Police):", coef(second_stage_police)["Crime_hat"], "\n")

# Direct 2SLS using AER package
tsls_police <- ivreg(Polic ~ Crime + RandBudg | Unemp + RandBudg, data = crime_data)
cat("Direct 2SLS (Crime effect on Police):", coef(tsls_police)["Crime"], "\n")

# Compare all results
cat("\n=== COMPARISON OF ESTIMATES ===\n")
cat("True effect of Police on Crime (b_c):", b_c, "\n")
cat("2SLS estimate:", coef(tsls_crime)["Polic"], "\n")

cat("\nTrue effect of Crime on Police (b_p):", b_p, "\n")
cat("2SLS estimate:", coef(tsls_police)["Crime"], "\n")