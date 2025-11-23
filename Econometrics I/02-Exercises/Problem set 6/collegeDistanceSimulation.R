# Instrumental Variables Laboratory

library(ivreg)
library(ggplot2)
library(AER)
library(stargazer)

# Parameters 
n_obs <- 100000 # no. of persons in the dataset (try first 100000)
gamma <- 1 # relevance of the instrument (play with values between .01 and 1)
set.seed(1234) # for replication purposes


# Simulations #############################################

cat("=== SIMULATING DATA ===\n")

talent <- runif(n_obs,0,10)                 # Person's talent (unobserved)
urban <- rbinom(n=n_obs, size=1, prob=0.5)  # Urban dummy

ldistance <- 1 -urban + rnorm(n_obs)        # Distance to college (in logs)
educ <- 6 - gamma*ldistance + talent + rnorm(n_obs) # Years of education
educ <- round(educ ,0) # drop decimals 

beta_true <- .05
lwage <- beta_true * educ + 0.5*urban + .02 * talent + rnorm(n_obs)  


# Estimations  #############################################

cat("=== ESTIMATING MODELS ===\n")

# column (1)
OLS <- lm(lwage ~ educ)

# column (2)
OLS_control <- lm(lwage ~ educ + urban)

# column (3)
IV <-  ivreg(lwage ~ educ | ldistance)

# column (4)
IV_controls <- ivreg(lwage ~ educ + urban | ldistance + urban)

# check first stage: 
IV_contr_first <- lm(educ ~ ldistance + urban )
summary(IV_contr_first)


stargazer(OLS,OLS_control,IV, IV_controls, type="text", omit.stat=c("ser","f"))

