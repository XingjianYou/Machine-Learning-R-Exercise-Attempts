# Chapter 13 Assignment

# 13.1 Boosting regression on concrete compressive strength
# (1) Select training set, fit gradient boosting model
rm(list=ls())
library(AppliedPredictiveModeling)
library(gbm)
data(concrete)
set.seed(1)
train_index <- sample(1030, 730)
train <- concrete[train_index,]
test  <- concrete[-train_index,]
set.seed(123)
fit <- gbm(CompressiveStrength ~ ., data=train, distribution="gaussian",
           n.trees=5000, cv.folds=5, interaction.depth=4)

# (2) Variable importance list and plot
summary(fit)

# (3) 1D and 2D partial dependence plots for Age and Cement
plot(fit, i.var="Age",    main="Partial Dependence Plot", ylab="CompressiveStrength")
plot(fit, i.var="Cement", main="Partial Dependence Plot", ylab="CompressiveStrength")
library(viridis)
plot(fit, i.var=c("Age","Cement"), main="Partial Dependence Plot")

# (4) CV error as a function of number of trees, with plot
gbm.perf(fit, method="cv")
abline(h=0, lty=2)
legend("top", legend=c("Training Error","CV Error"), lty=1, col=c("black","green"))

# (5) How many trees to reach minimum? What is that minimum?
min(fit$train.error)
which.min(fit$train.error)
min(fit$cv.error)
which.min(fit$cv.error)

# (6) Predict on test set and calculate MSE
pred   <- predict(fit, newdata=test, n.trees=5000)
y_test <- test[,"CompressiveStrength"]
mean((pred - y_test)^2)
# Try with CV-optimal number of trees
pred   <- predict(fit, newdata=test, n.trees=1041)
y_test <- test[,"CompressiveStrength"]
mean((pred - y_test)^2)

# 13.2 Gradient boosting for binary classification on German Credit data
# (1) Reserve 300 observations as test set
rm(list=ls())
# install.packages("evtree")
library(evtree)
data("GermanCredit")
set.seed(1)
train_index <- sample(1000, 700)

# (2) Convert all variables to dummy variables
GCg <- as.data.frame(model.matrix(~., GermanCredit)[,-1])
GCg <- GCg[,-31]
train <- GCg[train_index,]
test  <- GCg[-train_index,]

# (3) Fit gradient boosting model
library(gbm)
set.seed(123)
fit <- gbm(credit_riskbad ~ ., data=train, n.trees=1000, cv.folds=5,
           shrinkage=0.01, interaction.depth=2)

# (4) Examine training and CV error as function of number of trees
gbm.perf(fit, method="cv")
abline(h=0, lty=2)
legend("top", legend=c("Training Error","CV Error"), lty=1, col=c("black","green"))
min(fit$train.error)
which.min(fit$train.error)
min(fit$cv.error)
which.min(fit$cv.error)

# (5) Predict on test set, display confusion matrix and accuracy
prob   <- predict(fit, newdata=test, n.trees=1000, type="response")
pred   <- prob > 0.5
y_test <- test[,"credit_riskbad"]
(table      <- table(pred, y_test))
(error_rate <- 1 - sum(diag(table)) / sum(table))

prob   <- predict(fit, newdata=test, n.trees=855, type="response")
pred   <- prob > 0.5
(table      <- table(pred, y_test))
(error_rate <- 1 - sum(diag(table)) / sum(table))
