# In this homework we work with mulitple linear regression.
# We deal with multicollinearity by taking out some variables
# by hand and then doing the F test for model reduction.

# read the data
mselect.data <- read.csv("data/mselect.csv", header=TRUE)
plot(mselect.data)

# take out all variables which aren't signifigant
# (but keep any main effect if its involved in higher order term)
x2sq = mselect.data$x2^2
mselect.mod.ful <- lm(y ~ .*. + poly(x1, 2) + poly(x2, 2) + poly(x3, 2) + poly(x5, 2), data=mselect.data)
mselect.mod.mod1 <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x2:x5 + x3:x4 + x2sq, data=mselect.data)

# compare full mod and mod 1
# use the F-stat for reduced model versus full model
summary(mselect.mod.ful)
summary(mselect.mod.mod1)
anova(mselect.mod.mod1, mselect.mod.ful)

# check for multicollinearity using VIFs
vif(mselect.mod.mod1)

# backward using BIC
mselect.mod.mod2 = step(mselect.mod.ful, direction="backward", k=log(1000))

# make a model with what backward BIC chose
vif(mselect.mod.mod2)

