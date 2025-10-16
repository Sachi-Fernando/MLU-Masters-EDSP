# =============================================
# INTRODUCTORY R SCRIPT: The linear model
# =============================================


# ----------------------------
# PREAMBLE
# ----------------------------

rm(list=ls()) # This command deletes everything from your workspace. 

# Set working directory where you saved the data (will be different in your PC!)
setwd("G:/My Drive/UniHalle/Courses/Empirical Econ Analysis/ideas/1 linear trans and R2/")

# Load the required package
#install.packages("openxlsx") # installing the package (uncomment the first time)
library(openxlsx)  # For accessing excel files

#install.packages("stargazer") # installing the package (uncomment the first time)
library(stargazer) # For regression tables


# ----------------------------
# 1. LOADING AND VIEWING DATA
# ----------------------------

# Read Excel file
tech <- read.xlsx("tech_adoption.xlsx")

# View first few rows
head(tech)

# ----------------------------
# 2. BASIC PLOTTING
# ----------------------------

# Create scatter plot
plot(tech$Year, tech$Users, 
     main = "Tech Adoption Over Time",
     xlab = "Year", ylab = "Users",
     pch = 19, col = "blue")


# ----------------------------
# 3. LINEAR REGRESSION
# ----------------------------

# Run regression
model_linear <- lm(Users ~ Year, data = tech)

# Show results
summary(model_linear)

# ----------------------------
# 4. LOG TRANSFORMATION
# ----------------------------

# Plot log-transformed data
plot(tech$Year, log(tech$Users),
     main = "Log-Transformed Tech Adoption",
     xlab = "Year", ylab = "log(Users)",
     pch = 19, col = "red")

# Log-linear model
model_log <- lm(log(Users) ~ Year, data = tech)

# Show results
summary(model_log)

# Compare results
stargazer(model_linear, model_log, 
          type = "text",
          title = "Tech Adoption: Linear vs Log-Linear Models",
          column.labels = c("Linear", "Log-Linear"),
          dep.var.labels = c("Users", "log(Users)"))


# ----------------------------
# 5. POLYNOMIAL REGRESSION
# ----------------------------

# Read yield curve data
yield <- read.xlsx("yield_curve.xlsx")

# Basic plot
plot(yield$Maturity, yield$Yield,
     main = "Yield Curve",
     xlab = "Maturity", ylab = "Yield",
     pch = 19, col = "darkgreen")

# Cubic polynomial model
model_poly <- lm(Yield ~ Maturity + I(Maturity^2) + I(Maturity^3), 
                 data = yield)

# Show results
summary(model_poly)

# ----------------------------
# 6. MODEL COMPARISON
# ----------------------------

# Linear model for yield
model_linear_yield <- lm(Yield ~ Maturity, data = yield)


# Compare results
stargazer(model_linear_yield, model_poly,
          type = "text",
          title = "Yield Curve: Linear vs Polynomial Models",
          column.labels = c("Linear", "Cubic"),
          dep.var.labels = c("Yield", "Yield"))