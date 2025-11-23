#install.packages('wooldridge')
data(gpa1, package='wooldridge')

library(stargazer)

# Determine sample size & no. of regressors:
n <- nrow(gpa1); k<-2

# extract y x1 x2
y <- gpa1$colGPA
x2 <- gpa1$hsGPA
x3 <- gpa1$ACT

model <- lm(y ~ x2 + x3)
stargazer(model, type = "text")


# extract X & add a column of ones
X <- cbind(1, x2, x3)

# Display first rows of X:
head(X)

# Parameter estimates:
( betahat <- solve( t(X)%*%X ) %*% t(X)%*%y )

# Residuals, estimated variance of u and SSR:
uhat <- y - X %*% betahat
sigsqhat <- as.numeric( t(uhat) %*% uhat / (n-k-1) )
( SSR <- sqrt(sigsqhat) )

# Estimated variance of the parameter estimators and SE:
Vbetahat <- sigsqhat * solve( t(X)%*%X )

# display variances:
diag(Vbetahat)

# compute and display standard errors
( se <- sqrt( diag(Vbetahat) ) )

# compare
stargazer(model, type = "text")
