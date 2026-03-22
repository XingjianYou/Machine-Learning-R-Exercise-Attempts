# Chapter 15 Assignment

# 15.1 Predict Rings in abalone data
# (1) Convert factor variable to dummy variables
rm(list=ls())
setwd("/Users/Antony/OneDrive/CoreTech/ML&R/MLR_Data")
library(AppliedPredictiveModeling)
data("abalone")
summary(abalone)
dummy    <- model.matrix(~Type, data=abalone)
aba_data <- cbind(abalone[,-1], dummy[,-1])

# (2) Normalize all variables to [0,1]
maxs   <- apply(aba_data, 2, max)
mins   <- apply(aba_data, 2, min)
aba_s  <- as.data.frame(scale(aba_data, center=mins, scale=maxs-mins))

# (3) Select 1000 observations as training set
set.seed(1)
train_index <- sample(4177, 1000)
y_test      <- abalone[-train_index, "Rings"]
train <- aba_s[train_index,]
test  <- aba_s[-train_index,]
fit   <- neuralnet(Rings ~ ., data=train, hidden=3, act.fct="logistic")

# (4) Calculate MSE
plot(fit, fontsize=10)
pred <- predict(fit, test)
pred <- pred * (max(abalone$Rings) - min(abalone$Rings)) + min(abalone$Rings)
mean((pred - y_test)^2)

# (5) Compare with OLS regression MSE
fit_ols  <- lm(Rings ~ ., data=train)
pred_ols <- predict(fit_ols, test)
pred_ols <- pred_ols * (max(abalone$Rings) - min(abalone$Rings)) + min(abalone$Rings)
mean((pred_ols - y_test)^2)
par(mfrow=c(1,2))
plot(pred,     y_test, xlab="Prediction", ylab="Rings", main="Neural Network")
abline(0, 1)
plot(pred_ols, y_test, xlab="Prediction", ylab="Rings", main="OLS Regression")
abline(0, 1)
par(mfrow=c(1,1))

# (6) Find optimal number of hidden neurons via for loop over 1:10
MSE <- numeric(10)
for (i in 1:10) {
  set.seed(123)
  fit  <- neuralnet(Rings ~ ., data=train, hidden=i, act.fct="logistic")
  pred <- predict(fit, test)
  pred <- pred * (max(abalone$Rings) - min(abalone$Rings)) + min(abalone$Rings)
  MSE[i] <- mean((pred - y_test)^2)
}
min(MSE)
which.min(MSE)
plot(1:10, MSE, type="b", xlab="Hidden Neurons")

# 15.2 Binary classification on Ionosphere data from mlbench
# (1) Preprocessing: set class as factor, remove V2 (no variance),
#     convert two-level factor V1 to dummy variable
rm(list=ls())
library(mlbench)
data("Ionosphere")
Ionosphere <- subset(Ionosphere, select=-c(V2))
# Rename variable
library(reshape)
names(Ionosphere)[1] <- "V1-"
# Create dummy variables
library(caret)
str(Ionosphere)
dmi  <- dummyVars(~., data=Ionosphere)
dmio <- data.frame(predict(dmi, newdata=Ionosphere))

# (2) Select training set and fit model
set.seed(1)
train <- sample(351, 251)
fit   <- neuralnet(Class.good ~ ., data=dmio[train,], hidden=5, act.fct="logistic")

# (3) MSE on test set
prob  <- predict(fit, dmio[-train,])
pred  <- prob > 0.5
(table      <- table(Predicted=pred, Actual=dmio[-train,"Class.good"]))
(error_rate <- 1 - sum(diag(table)) / sum(table))

# (4) Compare with logistic regression
library(nnet)
fit_l <- multinom(Class.good ~ ., data=dmio[train,])
pred  <- predict(fit_l, type="class", newdata=dmio[-train,])
(table      <- table(Predicted=pred, Actual=dmio[-train,"Class.good"]))
(error_rate <- 1 - sum(diag(table)) / sum(table))

# (5) For loop over 1:10 hidden neurons
MSE <- numeric(10)
for (i in 1:10) {
  set.seed(123)
  fit  <- neuralnet(Class.good ~ ., data=dmio[train,], hidden=i, act.fct="logistic")
  prob <- predict(fit, dmio[-train,])
  pred <- prob > 0.5
  (table  <- table(Predicted=pred, Actual=dmio[-train,"Class.good"]))
  MSE[i] <- 1 - sum(diag(table)) / sum(table)
}
min(MSE)
which.min(MSE)
plot(1:10, MSE, type="b", xlab="Hidden Neurons")
