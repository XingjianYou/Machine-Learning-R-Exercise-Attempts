# Chapter 9 - Penalized Regression

# (1) Load data
rm(list=ls())
mat <- read.csv("student-mat.csv", sep=";")
str(mat)
summary(mat)

# (2) Remove two columns
smat <- mat[, !names(mat) %in% c("G1", "G2")]
summary(smat)
summary(mat)

# (3) Plot histogram of G3
table(smat$G3)
prop.table(table(smat$G3))
hist(smat$G3, main="Distribution of G3")

# (4) Ridge regression with 10-fold cross-validation to select optimal lambda
library(glmnet)
x <- model.matrix(G3 ~ ., smat[,])[,-1]
y <- smat$G3
rfit <- glmnet(x, y, alpha=0)
# 10-fold CV to select lambda
set.seed(1)
cv_rfit <- cv.glmnet(x, y, alpha=0)
plot(cv_rfit)
cv_rfit$lambda.min

# (5) Lasso regression
lfit <- glmnet(x, y, alpha=1)
plot(lfit, xvar="lambda", label=TRUE)
# Select lambda
cv_rfit <- cv.glmnet(x, y, alpha=1)
plot(cv_rfit)
cv_rfit$lambda.min

# (6) Elastic net regression
efit <- glmnet(x, y, alpha=0.5)
plot(lfit, xvar="lambda", label=TRUE)
# Select lambda
cv_rfit <- cv.glmnet(x, y, alpha=0.5)
plot(cv_rfit)
cv_rfit$lambda.min

# (7) For loop with 10-fold CV to select optimal alpha
foldid   <- sample(1:10, size=395, replace=TRUE)
cv_error <- numeric(11)
for(i in 1:11){
  cvfit      <- cv.glmnet(x, y, foldid=foldid, alpha=(i-1)/10)
  cv_error[i] <- min(cvfit$cvm)
}
cv_error
min(cv_error)
plot((1:11-1)/10, cv_error, type="b", xlab=expression(alpha), main="CV Error")

# (8) Reserve 100 observations as test set, select optimal elastic net parameters
set.seed(1)
train    <- sample(395, 295)
cvfit    <- cv.glmnet(x[train,], y[train], alpha=0)
best_lam <- cvfit$lambda.min
pred_train <- predict(efit, newx=x[train,], s=best_lam)
mean((pred_train - y[train])^2)

## Test error
pred_test <- predict(efit, newx=x[-train,], s=best_lam)
mean((pred_test - y[-train])^2)

## Test error with 1se
best_lam_1se  <- cvfit$lambda.1se
pred_test_1se <- predict(efit, newx=x[-train,], s=best_lam_1se)
mean((pred_test_1se - y[-train])^2)
