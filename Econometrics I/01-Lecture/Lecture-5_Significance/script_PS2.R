
# Set working directory where you saved the data (will be different in your PC!)
setwd("G:/My Drive/UniHalle/Courses/Econometrics 1 (master)/LECTURES/")

load("Lecture 5/br.RData")

# Plot price vs. sqft What kind of model or transformation would the scatter suggest? 

# levels
plot(br$sqft, br$price) 

# log-level
plot(br$sqft, log(br$price))

# log-log
plot(log(br$sqft), log(br$price))


model1 <- lm(price ~ sqft, data=br)
model2 <- lm(price ~ sqft + I(sqft^2), data=br)
model3 <- lm(log(price) ~ sqft, data=br)
model4 <- lm(log(price) ~ log(sqft), data=br)


library(stargazer) 
stargazer(model1, model2, model3, model4,
          type = "text")


# calculate fitted values and residuals 

# For model1 (linear)
br$fitted1 <- fitted(model1)
br$resid1 <- residuals(model1)

# For model2 (quadratic)
br$fitted2 <- fitted(model2)
br$resid2 <- residuals(model2)

# For model3 (log-linear)
br$fitted3 <- fitted(model3)
br$resid3 <- residuals(model3)

# For model4 (log-log)
br$fitted4 <- fitted(model4)
br$resid4 <- residuals(model4)



# Model 1
plot(br$sqft, br$resid1, main = "Model 1: Residuals vs Sqft",
     xlab = "Square Feet", ylab = "Residuals")
abline(h = 0, col = "red")

# Model 2
plot(br$sqft, br$resid2, main = "Model 2: Residuals vs Sqft",
     xlab = "Square Feet", ylab = "Residuals")
abline(h = 0, col = "red")

# Model 3
plot(br$sqft, br$resid3, main = "Model 3: Residuals vs Sqft",
     xlab = "Square Feet", ylab = "Residuals")
abline(h = 0, col = "red")

# Model 4
plot(br$sqft, br$resid4, main = "Model 4: Residuals vs Sqft",
     xlab = "Square Feet", ylab = "Residuals")
abline(h = 0, col = "red")



model5 <-  lm(log(price) ~ log(sqft)+bedrooms+baths+age, data=br)
model6 <-  lm(log(price) ~ log(sqft)+bedrooms+baths+age+pool+fireplace, data=br)


stargazer(model5, model6, type = "text")