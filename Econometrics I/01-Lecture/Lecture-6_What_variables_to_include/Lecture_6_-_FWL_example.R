setwd("G:/My Drive/UniHalle/Courses/Econometrics 1 (master)/LECTURES/Lecture 5/")


# ----------------------------
# 1. LOAD DATA and LIBRARIES
# ----------------------------


library(openxlsx)  # For accessing excel files
library(stargazer) # For regression tables
# Read Excel file
data <- read.xlsx("inequality.xlsx")


# ----------------------------
# 2. FWL theorem example
# ----------------------------

baseline <- lm(income2010 ~ ineq1950 +income1950	, data = data)

auxiliary <- lm(ineq1950 ~  +income1950	, data = data)
data$xtilde <- residuals(auxiliary)

FWL <- lm(income2010 ~ xtilde	, data = data)

# should produce the same coefficient:
stargazer(baseline, auxiliary, FWL,
          type = "text",
          keep.stat = c("n", "rsq", "ser"))


# Reported variance estimates differ, but in reality they are obviously equal. 