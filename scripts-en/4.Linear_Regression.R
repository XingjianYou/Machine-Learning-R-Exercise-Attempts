# Chapter 4 Assignment

# 4.4 Cobb-Douglas
# (1) Regress lny on lnk and lnl
rm(list=ls())
# Load data
data <- read.csv("cobb_douglas.csv", header=TRUE)
str(data)
attach(data)
summary(data)
# First regression
fit <- lm(lny ~ lnk + lnl, data=data)
summary(fit)

# (2) Plot
fitted_vals <- predict(fit, data)
plot(year, lny)
lines(year, fitted_vals, col="blue")

# (3) Add interaction term
fit_interaction <- lm(lny ~ lnk * lnl, data=data)
summary(fit_interaction)

# (4) Add squared terms
fit_squared <- lm(lny ~ lnk * lnl + I(lny^2) + I(lnk^2), data=data)
summary(fit_squared)

# 4.5 Predicting abalone age
# (1) Draw training set
rm(list=ls())
library(AppliedPredictiveModeling)
data(abalone)
abalone_data <- abalone
attach(abalone_data)
summary(abalone_data)
# Draw training set indices
set.seed(1)
train_index <- sample(4177, 3177)
test_set <- abalone_data[-train_index,]
train_set <- abalone_data[train_index,]

# (2) Regress using training set
fit <- lm(Rings ~ ., data=train_set) # "." means regress on all variables
summary(fit)

# (3) Predict on validation set
predicted <- predict(fit, test_set)
y_test <- test_set$Rings
MSE <- mean((predicted - y_test)^2)

# (4) Cross-validation
library(boot)
fit_cv <- glm(Rings ~ ., data=abalone_data)
cv_err <- cv.glm(abalone_data, fit_cv, K=10)
cv_err$delta

# (5) Leave-one-out cross-validation (LOOCV)
cv_err <- cv.glm(abalone_data, fit_cv)
cv_err$delta
