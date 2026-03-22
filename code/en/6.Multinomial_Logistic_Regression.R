# Chapter 6 Assignment

# (1) Structure of wine data and distribution of response variable
rm(list=ls())
# install.packages("rattle")
library(rattle)
data("wine")
attach(wine)
# Descriptive statistics and response variable distribution
str(wine)
summary(wine)
table(Type)
prop.table(table(Type))
# Control decimal places
options(digits=2)
prop.table(table(Type))

# (2) Train and test sets
train_index <- sample(178, 100)
train <- wine[train_index,]
test  <- wine[-train_index,]

# (3) Predict on test set
library(nnet)
fit <- multinom(Type ~ ., data=train)
options(digits=3)
summary(fit)
prob_test <- predict(fit, type="probs", newdata=test)
pred_test <- predict(fit, type="class", newdata=test)
table    <- table(Predicted=pred_test, Actual=test$Type)
Accuracy <- sum(diag(table)) / sum(table)
Accuracy

# (4) Calculate Kappa
library(vcd)
Kappa(table)
