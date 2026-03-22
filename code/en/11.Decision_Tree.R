# Chapter 11 Assignment

# 11.1 Regression on concrete compressive strength

# (1) Load data, fit regression decision tree
rm(list=ls())
library(AppliedPredictiveModeling)
data(concrete)
str(concrete)
summary(concrete)
set.seed(1)
train_index <- sample(1030, 730)
train <- concrete[train_index,]
test  <- concrete[-train_index,]
boxplot(concrete$CompressiveStrength)
library(rpart)
fit <- rpart(CompressiveStrength ~ ., data=concrete, subset=train_index)

# (2) Plot the estimated decision tree
op <- par(no.readonly=TRUE)
par(mar=c(5,5,5,5))
plot(fit, margin=0.1)
text(fit)
par(op)

# (3) Select optimal parameter and plot
plotcp(fit)
fit$cptable
min_cp   <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]
fit_best <- prune(fit, cp=min_cp)

# (5) Predict on test set and calculate MSE
tree_pred <- predict(fit_best, newdata=test)
mean((tree_pred - test$CompressiveStrength)^2)   # MSE
plot(tree_pred, test$CompressiveStrength, main="Tree Prediction")
abline(0, 1)

fit_1se       <- prune(fit, cp=0.02)
tree_pred_1se <- predict(fit_1se, newdata=test)
mean((tree_pred_1se - test$CompressiveStrength)^2)   # MSE

# (6) Test OLS error and compare
ols_fit  <- lm(CompressiveStrength ~ ., train)
ols_pred <- predict(ols_fit, test)
mean((ols_pred - test$CompressiveStrength)^2)   # MSE
plot(ols_pred, test$CompressiveStrength, main="OLS Prediction")
abline(0, 1)


# 11.2 Predicting mushroom classification
# (1) Split into train and test sets
install.packages("cba")
library(cba)
data(Mushroom)
str(Mushroom)
summary(Mushroom)
set.seed(123)
train_index <- sample(8124, 7124)
train <- Mushroom[train_index,]
test  <- Mushroom[-train_index,]
prop.table(table(Mushroom$class))
fit <- rpart(class ~ ., data=train)

# (2) Select optimal cp and plot
plotcp(fit)
fit$cptable
min_cp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]

# (3) Plot classification tree using optimal cp
fit_best <- prune(fit, cp=min_cp)
op <- par(no.readonly=TRUE)
par(mar=c(1,1,1,1))
plot(fit_best, margin=0.1)
text(fit_best, cex=1.5)
par(op)

# (4) Predict on test set, display confusion matrix, calculate accuracy
tree_pred <- predict(fit_best, test, type="class")
y_test    <- test$class
(table       <- table(tree_pred, y_test))
(accuracy    <- sum(diag(table)) / sum(table))
(sensitivity <- table[2,2] / (table[1,2] + table[2,2]))

# (5) Predict using information entropy
fit    <- rpart(class ~ ., data=train, parms=list(split="information"))
min_cp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "CP"]
fit_best  <- prune(fit, cp=min_cp)
tree_pred <- predict(fit_best, test, type="class")

# (6) Confusion matrix and accuracy
(table    <- table(tree_pred, y_test))
(accuracy <- sum(diag(table)) / sum(table))
