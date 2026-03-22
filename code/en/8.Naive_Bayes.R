# Chapter 8 Assignment

# (1) Load data from CSV
rm(list=ls())
train <- read.csv("land_train.csv", header=TRUE)
test  <- read.csv("land_test.csv",  header=TRUE)
str(train)
summary(train)
# Set class as factor
train$class <- factor(train$class)
test$class  <- factor(test$class)
table(train$class)
prop.table(table(train$class))
plot(train$class, main="Plot of Classes")

# (2) Fit Naive Bayes model
library(e1071)
fit <- naiveBayes(class ~ ., data=train)
summary(fit)

pred_train <- predict(fit, newdata=train)
(table <- table(Predicted=pred_train, Actual=train$class))

# (3) Predict on test set
pred_test <- predict(fit, newdata=test)
(table          <- table(Predicted=pred_test, Actual=test$class))
(test_error_rate <- 1 - sum(diag(table)) / sum(table))

# (4) Apply Laplace smoothing
fit1 <- naiveBayes(class ~ ., data=train, laplace=2)
pred_train <- predict(fit1, newdata=train)
(table <- table(Predicted=pred_train, Actual=train$class))

# (5) Predict on test set with Laplace smoothing, display confusion matrix
pred_test <- predict(fit1, newdata=test)
(table          <- table(Predicted=pred_test, Actual=test$class))
(test_error_rate <- 1 - sum(diag(table)) / sum(table))
