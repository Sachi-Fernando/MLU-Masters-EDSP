# Read the data
setwd("G:/My Drive/UniHalle/Courses/Econometrics 1 (master)/LECTURES/PS5 - binary, TS, panel, GLS/")

df <- read.table("passengers.txt", header = TRUE)

# Create log passengers
df$logPassengers <- log(df$passengers)

model1 <- lm(logPassengers ~ t, data = df)
df$simple_trend <- fitted(model1)

# Plot 
plot(df$t, df$logPassengers, type = "l", main = "Log Passengers", 
     xlab = "Time", ylab = "Log Passengers")
lines(df$t, df$simple_trend, col = "red")



# Create monthly dummies with simple loop
for(i in 1:12) {
  df[[paste0("dm", i)]] <- as.numeric(df$month == i)
}

# Get residuals
df$residuals <- residuals(model1)

# Create dummy-only model for residuals
model2 <- lm(residuals ~ dm1 + dm2 + dm3 + dm4 + dm5 + dm6 + 
                    dm7 + dm8 + dm9 + dm10 + dm11 + dm12, data = df)
df$dummy_fit <- fitted(model2)


# Plot 
plot(df$t, df$residuals, type = "l", main = "Log Passengers", 
     xlab = "Time", ylab = "Log Passengers")
lines(df$t, df$dummy_fit, col = "red")



# Time trend plus seasonal dummies model
model3 <- lm(logPassengers ~ t + dm1 + dm2 + dm3 + dm4 + dm5 + dm6 + 
              dm7 + dm8 + dm9 + dm10 + dm11 + dm12, data = df)



# Forecast next 24 months
last_t <- max(df$t)
future_t <- (last_t + 1):(last_t + 24)
future_months <- rep(1:12, 2)

forecast_data <- data.frame(t = future_t)
for(i in 1:12) {
  forecast_data[[paste0("dm", i)]] <- as.numeric(future_months == i)
}

forecast_logPassengers <- predict(model3, newdata = forecast_data)


# Plot 5: Two years forecast with preceding two years
last_2_years <- tail(df, 24)
plot(last_2_years$t, last_2_years$logPassengers, type = "l", 
     main = "2-Year Forecast vs Preceding 2 Years", 
     xlab = "Time", ylab = "Log Passengers",
     xlim = c(min(last_2_years$t), max(future_t)),
     ylim = range(c(last_2_years$logPassengers, forecast_logPassengers)))
lines(future_t, forecast_logPassengers, col = "red")