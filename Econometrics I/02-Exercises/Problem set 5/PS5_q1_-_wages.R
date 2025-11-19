
# =============================================
# Lecture 4a: Dummies
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


# ----------------------------
# High-school education
# ----------------------------


# Load data
url <- "http://www.stata.com/data/s4poe5/data/cps5_small.dta"
cps <- read_dta(url) # opens the datafile
head(cps)            # shows some of the data


# create a dummy for high school (i.e., educ > 9 years)
cps$highSchool <- ifelse(cps$educ > 9, 1, 0)
head(cps)            # shows some of the data


# create and plot log wages
cps$lwage <- log(cps$wage)
plot(cps$educ,cps$lwage,
     xlab = "Years of education", ylab = "Wage per hour (logs)")


# Estimate model HS1 to get means by group: 
model_HS1 <- lm(lwage ~  highSchool, data = cps)

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

# Estimate model HS2 with common slopes
model_HS2 <- lm(lwage ~ educ + highSchool , data = cps)
cps$predict_HS2 <- predict(model_HS2)

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

# Estimate model HS3 with differing slopes
model_HS3 <- lm(lwage ~ educ + highSchool + highSchool:educ, data = cps)
cps$predict_HS3 <- predict(model_HS3)

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



# ----------------------------
# Gender and education
# ----------------------------


# Model 1: Mean earnings of males and females
model1 <- lm(lwage ~ female, data = cps)
summary(model1)

# Predicted group means
av_male <- coef(model1)[1]
av_female <- coef(model1)[1] + coef(model1)[2]

# Plot for Model 1
plot1 <- ggplot(cps, aes(x = educ, y = lwage, color = factor(female))) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = av_male, color = "blue") +
  geom_hline(yintercept = av_female, color = "red") +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("female = 0", "female = 1")) +
  labs(title = "Model 1: Mean Log(Wage) by Gender",
       y = "Wage (in logs)", x = "Years of education", color = "Gender") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # x (left) and y (top)
        legend.justification = c(0, 1))  # anchor top-left

# Model 2: Same slope, different intercepts
model2 <- lm(lwage ~ female + educ, data = cps)
cps$predict2 <- predict(model2)

# Plot for Model 2
plot2 <- ggplot(cps, aes(x = educ, y = lwage, color = factor(female))) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = predict2), size = 1) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("female = 0", "female = 1")) +
  labs(title = "Model 2: Parallel Lines (Same Slope)",
       y = "Wage (in logs)", x = "Years of education", color = "Gender") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # x (left) and y (top)
        legend.justification = c(0, 1))  # anchor top-left



# Model 3: Independent slopes and intercepts
model3 <- lm(lwage ~ female + educ + female:educ, data = cps)
cps$predict3 <- predict(model3)

# Plot for Model 3
plot3 <- ggplot(cps, aes(x = educ, y = lwage, color = factor(female))) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = predict3), size = 1) +
  scale_color_manual(values = c("blue", "red"),
                     labels = c("female = 0", "female = 1")) +
  labs(title = "Model 3: Separate Slopes & Intercepts",
       y = "Wage (in logs)", x = "Years of education", color = "Gender") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # x (left) and y (top)
        legend.justification = c(0, 1))  # anchor top-left


# Model 4: model with black-female interaction
model4 <- lm(lwage ~ black + female + black:female, data = cps)
summary(model4)
cps$predict4 <- predict(model4)


# Create a grouping variable for race and gender combinations
cps$group <- interaction(cps$black, cps$female, sep = "_")

# Label the levels meaningfully
levels(cps$group) <- c("White Male", "White Female", "Black Male", "Black Female")

# Then plot using this variable
plot4 <- ggplot(cps, aes(x = educ, y = lwage, color = group)) +
  geom_point(alpha = 0.4) +
  geom_line(aes(y = predict4), size = 1) +
  scale_color_manual(values = c("blue", "red", "orange", "brown")) +
  labs(title = "Model 4: Black-Female Interaction",
       y = "Wage (in logs)", x = "Years of Education", color = "Group") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1))


# Regressions table
library(stargazer)
stargazer(model1, model2, model3, model4, type = "text",
          title = "Regression Results", 
          column.labels = c("Model 1", "Model 2", "Model 3"),
          dep.var.labels = "Log(Wage)", 
          no.space = TRUE, 
          keep.stat = c("n", "rsq"))


# ----------------------------
# Dummies as dependent variable
# ----------------------------
linearProbModel_1 <- lm(highSchool ~ black  , data = cps)
linearProbModel_2 <- lm(highSchool ~ black + female + black:female, data = cps)

stargazer(linearProbModel_1, linearProbModel_2, type = "text",
          title = "Regression Results", 
          no.space = TRUE, 
          keep.stat = c("n", "rsq"))





