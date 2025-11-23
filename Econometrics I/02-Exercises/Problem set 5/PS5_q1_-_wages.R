
# =============================================
# PS 5: Q1
# =============================================


# ----------------------------
# PREAMBLE
# ----------------------------

rm(list=ls()) # This command deletes everything from your workspace. 
# Load necessary libraries
library(haven)       # for reading Stata files
library(ggplot2)     # for plotting
library(dplyr)       # for data manipulation
library(broom)       # for working with regression outputs
library(ggthemes)    # for better plot styling
library(stargazer)

# ----------------------------
# High-school education 
# ----------------------------


# Load data
url <- "http://www.stata.com/data/s4poe5/data/cps5_small.dta"
cps <- read_dta(url) # opens the datafile
head(cps)            # shows some of the data


# (a) How do average salaries differ between those with and without high-school education?
# Answer after creating a high-school dummy indicating more than 9 years of
# education.

# create a dummy for high school (i.e., educ > 9 years)
cps$highSchool <- ifelse(cps$educ > 9, 1, 0)
head(cps)            # shows some of the data


# create and plot log wages
cps$lwage <- log(cps$wage)
plot(cps$educ,cps$lwage,
     xlab = "Years of education", ylab = "Wage per hour (logs)")


# Estimate model HS1 to get means by group: 
model_HS1 <- lm(lwage ~  highSchool, data = cps)

stargazer(model_HS1,type="text")

cps$predict_HS1 <- predict(model_HS1)

# Plot for Model HS1
plot_HS1 <- ggplot(cps, aes(x = educ, y = lwage, color = factor(highSchool))) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = predict_HS1), size = 1) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("No", "Yes")) +
  labs(y = "Wage (in logs)", x = "Years of education", color = "Visited high school") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # x (left) and y (top)
        legend.justification = c(0, 1))  # anchor top-left

plot_HS1


#(b) How do wages increase per year of education, considering that 
#  people with and without high school education have a different
#  initial salary, and forcing the same slope? 

# A: As we can see below, assuming a common slope, we obtain that having high 
# school education increases the salary by 10.8% 

# Estimate model HS2 with common slopes
model_HS2 <- lm(lwage ~ educ + highSchool , data = cps)
cps$predict_HS2 <- predict(model_HS2)

stargazer(model_HS1,model_HS2,type="text")


# Plot for Model HS2
plot_HS2 <- ggplot(cps, aes(x = educ, y = lwage, color = factor(highSchool))) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = predict_HS2), size = 1) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("No", "Yes")) +
  labs(y = "Wage (in logs)", x = "Years of education", color = "Visited high school") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # x (left) and y (top)
        legend.justification = c(0, 1))  # anchor top-left

plot_HS2

# (c) Does the wage increase per year depend on whether high school was visited?

# A: yes, it depends. Somebody with high school education experiences on average 
# a salary increase of 0.101+0.011 = 11.2% whereas those with no high school education
# experiences on average a salary increase of just 0.011 = 1.1%. 

# Estimate model HS3 with differing slopes
model_HS3 <- lm(lwage ~ educ + highSchool + highSchool:educ, data = cps)
cps$predict_HS3 <- predict(model_HS3)

stargazer(model_HS1,model_HS2,model_HS3,type="text")


# Plot for Model 5
plot_HS3 <- ggplot(cps, aes(x = educ, y = lwage, color = factor(highSchool))) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = predict_HS3), size = 1) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("No", "Yes")) +
  labs(y = "Wage (in logs)", x = "Years of education", color = "Visited high school") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # x (left) and y (top)
        legend.justification = c(0, 1))  # anchor top-left

plot_HS3

# Regressions table
library(stargazer)
stargazer(model_HS1, model_HS2, model_HS3, type = "text",
          title = "Regression Results", 
          dep.var.labels = "Log(Wage)", 
          no.space = TRUE, 
          order = c("highSchool", "educ", "educ:highSchool"),
          keep.stat = c("n", "rsq"))


# (d) How does the answer to (a) change if you distinguish between blacks and whites?
model_d <- lm(lwage ~  highSchool + black+ highSchool:black, data = cps)
stargazer(model_HS1, model_d, type = "text",
          title = "Regression Results", 
          dep.var.labels = "Log(Wage)", 
          no.space = TRUE, 
          keep.stat = c("n", "rsq"))


# (e) How does the chance of having high-school education depend on being black?

model_e <- lm(highSchool ~   black, data = cps)
stargazer(model_e, type = "text",
          no.space = TRUE, 
          keep.stat = c("n", "rsq"))


# A: The probability of having high school education is 0.2% lower for black 
# people but that estimate is not significantly different from zero. 


# (f)
model_f <- lm(highSchool ~   black + female + female:black, data = cps)
stargazer(model_d, type = "text",
          no.space = TRUE, 
          keep.stat = c("n", "rsq"))

# A: Females have a 3.6% higher chance to have high school education. The interaction
 # with black is not statistically significant. 





