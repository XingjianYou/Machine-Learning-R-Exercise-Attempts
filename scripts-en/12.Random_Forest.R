# Chapter 12 Assignment

# 12.1 Regression on concrete data
# (1) Load and split data, fit random forest regression
rm(list=ls())
library(AppliedPredictiveModeling)
data("concrete")

set.seed(1)
train_index <- sample(1030, 730)
train <- concrete[train_index,]
test  <- concrete[-train_index,]
summary(concrete)

# Fit random forest regression
set.seed(123)
library(randomForest)
fit <- randomForest(CompressiveStrength ~ ., data=train, mtry=9, importance=TRUE)
fit

# (2) Variable importance list and plot
importance(fit)
varImpPlot(fit, main="Variable Importance Plot")

# (3) Partial dependence plots for Age and Cement
partialPlot(fit, train, x.var=Age)
partialPlot(fit, train, x.var=Cement)

# (4) Predict on test set and display MSE
fit_pred <- predict(fit, newdata=test)
mean((fit_pred - test$CompressiveStrength)^2)

# (5) Find optimal mtry parameter
# Minimize OOB error
MSE <- numeric(9)
set.seed(123)
for (i in 1:9) {
  fit    <- randomForest(CompressiveStrength ~ ., data=train, mtry=i)
  MSE[i] <- mean(fit$mse[500])
}
which.min(MSE)
plot(1:9, MSE, type="b", xlab="mtry", main="OOB Errors")
# 10-fold cross-validation
foldid <- sample(1:10, size=1030, replace=TRUE)
head(foldid)
MSE <- matrix(rep(0, 80), ncol=10)
for (i in 1:8) {
  for (j in 1:10) {
    train_cv <- concrete[foldid != j,]
    holdout  <- concrete[foldid == j,]
    fit      <- randomForest(CompressiveStrength ~ ., data=train_cv, mtry=i)
    pred     <- predict(fit, newdata=holdout)
    y_test   <- holdout[,"CompressiveStrength"]
    MSE[i,j] <- mean((pred - y_test)^2)
  }
}
cv_error <- apply(MSE, 1, mean)
min(cv_error)
which.min(cv_error)
plot(1:8, cv_error, type="b", xlab="mtry", main="CV Error")
abline(v=which.min(cv_error), lty=2)

# (6) Select mtry via test set error
MSE <- numeric(8)
set.seed(123)
for (i in 1:8) {
  fit    <- randomForest(CompressiveStrength ~ ., data=train, mtry=i)
  pred   <- predict(fit, newdata=test)
  y_test <- test$CompressiveStrength
  MSE[i] <- mean((pred - y_test)^2)
}
min(MSE)
which.min(MSE)
plot(1:8, MSE, type="b", xlab="mtry", main="Test Error")
abline(v=which.min(MSE), lty=2)

# 12.2 Mushroom classification
rm(list=ls())
library(cba)
library(randomForest)
data("Mushroom")

# (1) Check for missing values
sum(is.na(Mushroom))
summary(Mushroom)

# (2) Impute missing values
Mushroom <- na.roughfix(Mushroom)

# (3) Fix variable names
str(Mushroom)
names(Mushroom) <- make.names(names(Mushroom))

# (4) Split train/test sets
set.seed(1)
train_index <- sample(8124, 7124)
train <- Mushroom[train_index,]
test  <- Mushroom[-train_index,]
fit <- randomForest(class ~ ., data=train, importance=TRUE)
varImpPlot(fit, main="Variable Importance Plot")

# (5) Predict on test set
pred  <- predict(fit, newdata=test)
table <- table(pred, test$class)
table
(error_rate <- 1 - sum(diag(table)) / sum(table))
