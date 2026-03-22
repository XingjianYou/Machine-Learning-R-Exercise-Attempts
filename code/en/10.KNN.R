# Chapter 10 Assignment

# (1) Display data structure, examine distribution of response variable
rm(list=ls())
library(FNN)
library(mlbench)
data(PimaIndiansDiabetes)
# Display data structure
summary(PimaIndiansDiabetes)
head(PimaIndiansDiabetes)
attach(PimaIndiansDiabetes)
table(diabetes)
prop.table(table(diabetes))

# (2) Plot boxplot
boxplot(mass ~ diabetes, main='Boxplot for mass')

# (3) Standardize all variables
pidz <- as.data.frame(scale(PimaIndiansDiabetes[-9]))
apply(pidz, 2, mean)
apply(pidz, 2, sd)

# (4) Split test set, estimate with K=10
train_index <- sample(768, 768-200)
test_label  <- PimaIndiansDiabetes[-train_index,]$diabetes
train <- pidz[train_index,]
test  <- pidz[-train_index,]
pred  <- knn(train=train, test=test, cl=PimaIndiansDiabetes[train_index,]$diabetes, k=10, prob=TRUE)

# (5) Predict and display confusion matrix
(table    <- table(pred, test_label))
(accuracy <- sum(diag(table)) / sum(table))

# (6) Vary parameter to find optimal k
accuracy <- numeric(50)
for (i in 1:50) {
  pred        <- knn(train=train, test=test, cl=PimaIndiansDiabetes[train_index,]$diabetes, k=i, prob=TRUE)
  table       <- table(pred, test_label)
  accuracy[i] <- sum(diag(table)) / sum(table)
}
max(accuracy)
which.max(accuracy)
plot(accuracy, type="b", xlab="K", ylab="Accuracy", main="Test Set Accuracy")
