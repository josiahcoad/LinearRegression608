# In this problem we work with mulitple linear regression.
# We deal with multicollinearity, skewed data/outliers and nonnormality.
# We deal with multicollinearity through various methods of 
# model reduction using BIC, AIC, AICc and p-val statistics
# and using methods of forward, backward, stepwise, subsets and LASSO.

library(MASS)
library(car)
library(leaps)
library(My.stepwise)
library(lars)

# read the data
pgatour.data <- read.csv("code/pgatour2006.csv", header=TRUE)
attach(pgatour.data)

# data exploration
plot(pgatour.data) # notice heavy right skew of prize money data

# make linear model
lm<-lm(PrizeMoney ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound)

# plot diagnostic plots
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfrow=c(2,2))
plot(lm)    # notice non-normality, high cooks distance
summary(lm) # notice some variables are not signifigant
vif(lm)     # notice vif > 5. Serious problems with multicollinearity

# run box cox
X <- cbind(DrivingAccuracy, GIR, PuttingAverage, BirdieConversion, SandSaves, Scrambling, PuttsPerRound)
transx = powerTransform(X)
summary(transx) # notice the pval for no transformation... we need to transform!

# fit lm on logy
lm.logy = lm(log(PrizeMoney) ~ DrivingAccuracy + GIR + PuttingAverage + BirdieConversion + SandSaves + Scrambling + PuttsPerRound)
vif(lm.logy) # still problems with multicollinearity
par(cex.axis=2,cex.lab=2, mar=c(5.1,5.1,2,2),lwd=2, pch=19, mfrow=c(2,2))
plot(lm.logy)
summary(lm.logy) # still non-sig variables

add_conservative_cooks_line <- function(model){
  dc <- 4/model$df.residual
  h <- seq(0.01, max(hatvalues(model))*1.1, by = 0.01)
  r <- sqrt(length(model$coefficients) * dc * (1-h)/h)
  lines(h, r, col ='purple', lty='dotdash')
  lines(h, -r, col ='purple', lty='dotdash')
}
add_conservative_cooks_line(lm.logy)
k = cooks.distance(lm.logy)
plot(k) # still a high cooks distance but not as bad


# ---- START MODEL REDUCTION ----#


# get the best possible subsets for each num of variables (stat used doesn't matter)
par(mfrow=c(1,1))
X <- cbind(DrivingAccuracy, GIR, PuttingAverage, BirdieConversion, SandSaves, Scrambling, PuttsPerRound)
b<-regsubsets(X, log(PrizeMoney))
summary(b)

# find best possible subset using adj2 and bic
subsets(b, statistic=c("adjr2"))
subsets(b, statistic=c("bic"))

# find best possible subset using AICc
# must do manually becuase not a valid criteria for 'subsets'
lm1 = lm(log(PrizeMoney) ~ GIR)
lm2 = lm(log(PrizeMoney) ~ GIR + PuttsPerRound)
lm3 = lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling)
lm4 = lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling + SandSaves )
lm5 = lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling + SandSaves + PuttsPerRound)
lm6 = lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling + SandSaves + PuttsPerRound + DrivingAccuracy)
lm7 = lm(log(PrizeMoney) ~ GIR + BirdieConversion + Scrambling + SandSaves + PuttsPerRound + DrivingAccuracy + PuttingAverage)


#Calculate AICc for each model
n <- length(lm1$residuals)

K <- length(lm1$coefficients) + 1
extractAIC(lm1,k=2)+2*K*(K+1)/(n-K-1)
K <- length(lm2$coefficients) + 1
extractAIC(lm2,k=2)+2*K*(K+1)/(n-K-1)
K <- length(lm3$coefficients) + 1
extractAIC(lm3,k=2)+2*K*(K+1)/(n-K-1)
K <- length(lm4$coefficients) + 1
extractAIC(lm4,k=2)+2*K*(K+1)/(n-K-1)
K <- length(lm5$coefficients) + 1
extractAIC(lm5,k=2)+2*K*(K+1)/(n-K-1)
K <- length(lm6$coefficients) + 1
extractAIC(lm6,k=2)+2*K*(K+1)/(n-K-1)
K <- length(lm7$coefficients) + 1
extractAIC(lm7,k=2)+2*K*(K+1)/(n-K-1)

# -- Now use forward and backward selection methods -- #

# backward using BIC
step(lm.logy, direction="backward", k=log(196))

# backward using AIC
step(lm.logy, direction="backward")

# forward using BIC
base = lm(log(PrizeMoney)~1)
step(base, scope=list(lower=base, upper=lm.logy), direction="forward", k=log(196))

# forward using AIC
step(base, scope=list(lower=base, upper=lm.logy), direction="forward")


# ---- LASSO ----

# automatic normalization (Put on [0, 1] scale)
mlasso<-lars(X, log(PrizeMoney), type="lasso", normalize = FALSE, trace=TRUE)
summary.lars(mlasso)
coef(mlasso)
print(mlasso)

par(mfrow=c(1,1), mar=c(5,5,4,4), cex.axis=0.8)
plot.lars(mlasso, xvar="step")
