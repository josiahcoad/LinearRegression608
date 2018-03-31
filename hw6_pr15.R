# In this homework we work with mulitple linear regression.
# We deal with again with multicollinearity
# using AIC and BIC on a much smaller data set.

library(MASS)
library(car)
library(leaps)
library(My.stepwise)
library(lars)

# See question 1 of Chapter 7, p. 252.
mantel.data <- read.table("code/Mantel.txt", header=TRUE)
plot(mantel.data)
mantel.mod = lm(Y ~ X1 + X2 + X3, data=mantel.data)

X<-cbind(mantel.data$X1, mantel.data$X2, mantel.data$X3)
b<-regsubsets(X, mantel.data$Y)
summary(b)

par(mfrow=c(1,1))
subsets(b, statistic=c("adjr2"))
subsets(b, statistic=c("bic"))

# backward using BIC
step(lm.logy, direction="backward", k=log(5))

# backward using AIC
step(lm.logy, direction="backward")

# forward using BIC
base = lm(Y~1)
step(base, scope=list(lower=base, upper=mantel.mod), direction="forward", k=log(5))

# forward using AIC
step(base, scope=list(lower=base, upper=mantel.mod), direction="forward")