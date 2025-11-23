setwd("G:/My Drive/UniHalle/Courses/Econometrics 1 (master)/LECTURES/Lecture 5/")

library(openxlsx)  # For accessing excel files
library(stargazer) # For regression tables


# ----------------------------
# 1. LOADING AND VIEWING DATA
# ----------------------------

# Read Excel file
data <- read.xlsx("inequality.xlsx")

# rescaling

data$income2010 <- data$income2010/1000000
data$income1950 <- data$income1950/1000000
# ----------------------------
# 3. LINEAR REGRESSIONS
# ----------------------------

# Run regression
model1 <- lm(income2010 ~ income1950	, data = data)
model2 <- lm(income2010 ~ ineq1950	, data = data)
model3 <- lm(income2010 ~ ineq1950 +income1950	, data = data)

# Compare results
stargazer(model1, model2, 
          type = "text",
          title = "Inequality vs. income",
          keep.stat=c("n","rsq")
)


dataSmaller <- subset(data, i < 7)
model4 <- lm(income2010 ~ income1950 	, data = dataSmaller)
model5 <- lm(income2010 ~ ineq1950 	, data = dataSmaller)
model6 <- lm(income2010 ~ ineq1950 +income1950	, data = dataSmaller)


# Compare results
stargazer(model1, model2, model4,  model5, model6, model3,
          type = "text",
          title = "Inequality vs. income",
          keep.stat=c("n","rsq")
          )
