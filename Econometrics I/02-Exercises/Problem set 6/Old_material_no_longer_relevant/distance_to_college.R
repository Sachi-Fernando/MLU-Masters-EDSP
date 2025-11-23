# College Distance Instrumental Variables Analysis
# Based on David Card's 1995 study

# Load required packages
library(AER)
library(ggplot2)
library(stargazer)
library(xtable)

# Load the dataset
data("CollegeDistance")

# Explore the data
cat("=== DATA DESCRIPTION ===\n")
str(CollegeDistance)
summary(CollegeDistance)

# Key variables:
# wage: log hourly wage
# education: years of education  
# distance: distance to nearest college (instrument)
# ethnicity, gender, income, etc.

# Create some factor variables for better interpretation
CollegeDistance$female <- ifelse(CollegeDistance$gender == "female", 1, 0)
CollegeDistance$black <- ifelse(CollegeDistance$ethnicity == "afam", 1, 0)
CollegeDistance$hispanic <- ifelse(CollegeDistance$ethnicity == "hispanic", 1, 0)

###############################################################################
# QUESTION (a): Simple OLS regression
###############################################################################

cat("\n=== QUESTION (a): SIMPLE OLS REGRESSION ===\n")

ols_simple <- lm(wage ~ education, data = CollegeDistance)
summary(ols_simple)

cat("OLS estimate of returns to education:", 
    round(coef(ols_simple)["education"], 4), "\n")
cat("Interpretation (assuming wages are in logs): One additional year of education is associated with a",
    round(coef(ols_simple)["education"] * 100, 2), "% increase in wages\n")

###############################################################################
# QUESTION (b): Instrument validity discussion
###############################################################################

cat("\n=== QUESTION (b): INSTRUMENT VALIDITY ===\n")

# Check relevance: relationship between distance and education
cat("Correlation between distance and education:", 
    round(cor(CollegeDistance$distance, CollegeDistance$education, use = "complete.obs"), 3), "\n")

# Visualize the first stage relationship
ggplot(CollegeDistance, aes(x = distance, y = education)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "First Stage: Distance to College vs Years of Education",
       x = "Distance to Nearest College",
       y = "Years of Education") +
  theme_minimal()

###############################################################################
# QUESTION (c): IV estimation and comparison
###############################################################################

cat("\n=== QUESTION (c): IV ESTIMATION ===\n")

# Simple IV with distance as instrument
iv_simple <- ivreg(wage ~ education | distance, data = CollegeDistance)
summary(iv_simple)

cat("IV estimate of returns to education:", 
    round(coef(iv_simple)["education"], 4), "\n")
cat("IV vs OLS difference:", 
    round(coef(iv_simple)["education"] - coef(ols_simple)["education"], 4), "\n")

# Create comparison table
models <- list(OLS = ols_simple, IV = iv_simple)
stargazer(models, type = "text", 
          title = "Comparison of OLS and IV Estimates",
          keep = "education",
          keep.stat = c("n", "rsq"))

###############################################################################
# QUESTION (d): Instrument strength test
###############################################################################

cat("\n=== QUESTION (d): INSTRUMENT STRENGTH ===\n")

# First stage regression
first_stage <- lm(education ~ distance, data = CollegeDistance)
summary(first_stage)

# Calculate F-statistic for instrument strength
f_stat <- summary(first_stage)$fstatistic[1]
cat("First stage F-statistic:", round(f_stat, 2), "\n")

if(f_stat > 10) {
  cat("Instrument is strong (F > 10)\n")
} else {
  cat("Warning: Instrument may be weak (F < 10)\n")
}


###############################################################################
# QUESTION (e): Models with control variables
###############################################################################

cat("\n=== QUESTION (e): MODELS WITH CONTROLS ===\n")

# OLS with controls
ols_controls <- lm(wage ~ education + black + hispanic + female + income + urban + region, 
                   data = CollegeDistance)

# IV with controls
iv_controls <- ivreg(wage ~ education + black + hispanic + female + income + urban + region | 
                       distance + black + hispanic + female + income + urban + region, 
                     data = CollegeDistance)

# First stage with controls
first_stage_controls <- lm(education ~ distance + black + hispanic + female + income + urban + region, 
                           data = CollegeDistance)

cat("First stage F-statistic with controls:", 
    round(summary(first_stage_controls)$fstatistic[1], 2), "\n")

# Compare all models
models_complete <- list(OLS_Simple = ols_simple, 
                        IV_Simple = iv_simple,
                        OLS_Controls = ols_controls,
                        IV_Controls = iv_controls)

stargazer(models_complete, type = "text",
          title = "Complete Model Comparison",
          keep = "education",
          keep.stat = c("n", "rsq"))

###############################################################################
# QUESTION 6: Testing exclusion restriction concerns
###############################################################################

cat("\n=== QUESTION 6: EXCLUSION RESTRICTION TESTS ===\n")

# Test balance: Compare characteristics by distance quartiles
CollegeDistance$dist_quartile <- cut(CollegeDistance$distance, 
                                     breaks = quantile(CollegeDistance$distance, 
                                                       probs = c(0, 0.25, 0.5, 0.75, 1)),
                                     labels = c("Q1", "Q2", "Q3", "Q4"))

balance_table <- aggregate(cbind(black, hispanic, female, income, urban) ~ dist_quartile, 
                           data = CollegeDistance, 
                           FUN = mean)

print(balance_table)

# Placebo test: Check if distance affects wages for older cohorts (if available)
# or test for direct effects

# Test for direct effect of distance on wages (should be insignificant in proper specification)
direct_effect_test <- lm(wage ~ distance + education + black + hispanic + female + income + urban, 
                         data = CollegeDistance)
cat("Direct effect of distance on wages (controlling for education):", 
    round(coef(direct_effect_test)["distance"], 4), "\n")
cat("P-value:", round(summary(direct_effect_test)$coefficients["distance", 4], 4), "\n")

###############################################################################
# QUESTION 7: LATE interpretation
###############################################################################

cat("\n=== QUESTION 7: LATE INTERPRETATION ===\n")

# Characterize compliers by estimating first stage for different subgroups
first_stage_black <- lm(education ~ distance, 
                        data = CollegeDistance[CollegeDistance$black == 1,])
first_stage_nonblack <- lm(education ~ distance, 
                           data = CollegeDistance[CollegeDistance$black == 0,])

first_stage_lowinc <- lm(education ~ distance, 
                         data = CollegeDistance[CollegeDistance$income == "low",])
first_stage_highinc <- lm(education ~ distance, 
                          data = CollegeDistance[CollegeDistance$income == "high",])

cat("First stage for black students:", 
    round(coef(first_stage_black)["distance"], 4), "\n")
cat("First stage for non-black students:", 
    round(coef(first_stage_nonblack)["distance"], 4), "\n")
cat("First stage for low income:", 
    round(coef(first_stage_lowinc)["distance"], 4), "\n")
cat("First stage for high income:", 
    round(coef(first_stage_highinc)["distance"], 4), "\n")

###############################################################################
# ADDITIONAL DIAGNOSTICS AND VISUALIZATIONS
###############################################################################

cat("\n=== ADDITIONAL DIAGNOSTICS ===\n")

# Distribution of key variables
par(mfrow = c(2, 2))
hist(CollegeDistance$wage, main = "Distribution of Wages", xlab = "Log Wage")
hist(CollegeDistance$education, main = "Distribution of Education", xlab = "Years")
hist(CollegeDistance$distance, main = "Distribution of Distance", xlab = "Miles")
plot(CollegeDistance$education, CollegeDistance$wage, 
     xlab = "Education", ylab = "Wage",
     main = "Education vs Wage")
par(mfrow = c(1, 1))

# Reduced form relationship
reduced_form <- lm(wage ~ distance, data = CollegeDistance)
cat("Reduced form coefficient (distance -> wage):", 
    round(coef(reduced_form)["distance"], 4), "\n")

# Wald estimator manually
reduced_form_coef <- coef(reduced_form)["distance"]
first_stage_coef <- coef(first_stage)["distance"]
wald_estimator <- reduced_form_coef / first_stage_coef
cat("Wald estimator:", round(wald_estimator, 4), "\n")
cat("IV coefficient:", round(coef(iv_simple)["education"], 4), "\n")

# Overidentification test (if we had multiple instruments)
cat("\nNote: For overidentification test, we would need multiple instruments.\n")
cat("This could include distance to 2-year vs 4-year colleges, etc.\n")

###############################################################################
# SUMMARY OF RESULTS
###############################################################################

cat("\n=== SUMMARY OF KEY RESULTS ===\n")
cat("1. OLS estimate of returns to education:", 
    round(coef(ols_simple)["education"], 4), "\n")
cat("2. IV estimate of returns to education:", 
    round(coef(iv_simple)["education"], 4), "\n")
cat("3. First stage F-statistic:", round(f_stat, 2), "\n")
cat("4. Instrument strength:", ifelse(f_stat > 10, "Strong", "Weak"), "\n")
cat("5. IV > OLS suggests: OLS may be biased downward\n")
cat("6. Possible explanations: Measurement error or heterogeneous returns\n")

# Save workspace for later use
save.image("CollegeDistance_analysis.RData")
cat("\nAnalysis complete. Workspace saved to CollegeDistance_analysis.RData\n")