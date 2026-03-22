# Chapter 7 Assignment

# (1) Load data, set response variable as factor, display data structure
rm(list=ls())
seeds <- read.table("seeds_dataset.txt", header=TRUE)
str(seeds)
summary(seeds)
# Set type as factor variable
seeds$X1 <- factor(seeds$X1)
table(seeds$X1)
prop.table(table(seeds$X1))

# (2) Split into train and test sets
set.seed(1)
train_index <- sample(209, 209*(2/3))
train <- seeds[train_index,]
test  <- seeds[-train_index,]

# (3) Plot scatter plot of first and second linear discriminants
library(MASS)
fit <- lda(X1 ~ ., data=train)
fit
summary(fit)
plot(fit, abbrev=TRUE, col=as.numeric(train$X1), main="Linear Discriminants")

# (4) Predict on test set, display confusion matrix
class_test <- predict(fit, newdata=test)$class
table <- table(Predicted=class_test, Actual=test$X1)
table
cat("LDA Test Error Rate =", mean(class_test != test$X1), "\n")
# Cohen's Kappa
library(vcd)
Kappa(table)

# (5) Perform Quadratic Discriminant Analysis
fit <- qda(X1 ~ ., data=seeds)
fit

# (6) Confusion matrix and accuracy for QDA on test set
class_test <- predict(fit, newdata=test)$class
table(Predicted=class_test, Actual=test$X1)
table
(Accuracy <- (table[1,1] + table[2,2]) / sum(table))
