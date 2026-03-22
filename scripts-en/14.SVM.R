# Chapter 14 Assignment

# 14.1 Multi-class SVM on UCI ultrasonic flow meter dataset (Meter_D),
#       response variable is the meter's four operating states
# (1) Load data, set variable 44 as factor
rm(list=ls())
meter <- read.table("Meter_D.csv", sep=",", header=TRUE)
meter$V44 <- factor(meter$V44)

# (2) Randomly select 100 observations as training set
set.seed(1)
train_index <- sample(180, 100)
train <- meter[train_index,]
test  <- meter[-train_index,]
str(train)
prop.table(table(train$V44))
fit  <- svm(V44 ~ ., data=train, kernel="linear", cost=0.5)
pred <- predict(fit, test)
(table      <- table(Predicted=pred, Actual=test$V44))
(test_error <- 1 - sum(diag(table)) / sum(table))

# (3) Fit SVM with polynomial kernel
fit  <- svm(V44 ~ ., data=train, kernel="polynomial", cost=0.5, gamma=0.5)
pred <- predict(fit, test)
(table      <- table(Predicted=pred, Actual=test$V44))
(test_error <- 1 - sum(diag(table)) / sum(table))

# (4) Fit SVM with radial basis kernel
fit  <- svm(V44 ~ ., data=train, kernel="radial", cost=1, gamma=1)
pred <- predict(fit, test)
(table      <- table(Predicted=pred, Actual=test$V44))
(test_error <- 1 - sum(diag(table)) / sum(table))

# (5) Select optimal cost via cross-validation, calculate best model accuracy
tune_out <- tune(svm, V44 ~ ., data=train, kernel="linear",
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
tune_out
fit_best <- tune_out$best.model
pred     <- predict(fit_best, test)
table    <- table(Predicted=pred, Actual=test$V44)
(test_error <- 1 - sum(diag(table)) / sum(table))

# (6) Select optimal cost for linear kernel via test set accuracy, plot results
test_out <- tune(svm, V44 ~ ., data=train,
                 validation.x=test[, names(test) != "V44"], validation.y=test[,"V44"],
                 kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
test_out
plot(test_out, xlab="C", ylab="Test Error", main="Test Error of Test Dataset")

# 14.2 Multi-class SVM on white wine quality data (winequality-white.csv),
#       response variable quality is wine rating from 1-10 (3-9 in sample)
# (1) Read data with read.table(), set quality as factor
white <- read.table("winequality-white.csv", sep=";", header=TRUE)
white$quality <- as.factor(white$quality)

# (2) Examine distribution of quality and plot bar chart
prop.table(table(white$quality))
plot(white$quality, xlab="Level", ylab="Count", main="Distribution of Levels")

# (3) Use set.seed(1), select 1000 observations as test set
set.seed(1)
train_index <- sample(4898, 4898-1000)
train <- white[train_index,]
test  <- white[-train_index,]

# (4) Fit SVM, display confusion matrix
fit  <- svm(quality ~ ., data=train, gamma=1, cost=1)
pred <- predict(fit, test)
(table      <- table(Predicted=pred, Actual=test$quality))
(test_error <- 1 - sum(diag(table)) / sum(table))

# (5) Compare accuracy with multinomial logistic regression
library(nnet)
fit  <- multinom(quality ~ ., data=train, maxit=500)
pred <- predict(fit, test)
(table      <- table(Predicted=pred, Actual=test$quality))
(test_error <- 1 - sum(diag(table)) / sum(table))
