# Load required packages
library(plm)
library(stargazer)
library(openxlsx)  # For accessing excel files

# Read Excel file
setwd("G:/My Drive/UniHalle/Courses/Econometrics 1 (master)/LECTURES/PS5 - binary, TS, panel, GLS/")
df <- read.xlsx("lifeexpetancyPanel.xlsx")

# transform into log
df$gdppercapita = log(df$gdppercapita)

# Manual Fixed Effects with dummy variables
pooled_model <- lm(lifeexpectancy ~ gdppercapita , 
                data = df)

# Manual Fixed Effects with dummy variables
fe_manual <- lm(lifeexpectancy ~ gdppercapita + factor(country) , 
                data = df)

# Estimate Fixed Effects model
fe_model <- plm(lifeexpectancy ~ gdppercapita, 
                data = df, 
                index = c("country", "decade"), 
                model = "within")

# Estimate Random Effects model  
re_model <- plm(lifeexpectancy ~ gdppercapita,
                data = df,
                index = c("country", "decade"),
                model = "random")

# Hausman test to compare FE vs RE
hausman_test <- phtest(fe_model, re_model)

# Display results
stargazer(pooled_model, fe_manual, fe_model, re_model, 
          type = "text",
          column.labels = c("Pooled OLS", "FE dummies", "FE plm", "RE plm"),
          dep.var.labels = "Life Expectancy",
          keep.stat = c("n", "rsq"))


