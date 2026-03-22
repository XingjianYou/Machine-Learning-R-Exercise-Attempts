# Chapter 5 Assignment

# (1) Set chd as factor, calculate proportion of coronary heart disease in sample
rm(list=ls())
# install.packages("ElemStatLearn")  # installed locally
library(ElemStatLearn)
# Load and descriptive statistics
data("SAheart")
summary(SAheart)
# Calculate proportion with coronary heart disease
summary(SAheart)
head(SAheart)
SAheart$chd <- factor(SAheart$chd)
str(SAheart)
summary(SAheart)

attach(SAheart)
prop.table(table(chd))

# (2) Split into train/test sets
set.seed(1)
train_index <- sample(462, 362)
train <- SAheart[train_index,]
test <- SAheart[-train_index,]

# (3) Logistic regression on training set
logit_fit <- glm(chd ~ ., data=train, family=binomial)
summary(logit_fit)

# (4) Calculate pseudo R-squared
names(logit_fit)
pseudo_r2 <- ((logit_fit$null.deviance - logit_fit$deviance) / logit_fit$null.deviance)
pseudo_r2

# (5) Calculate average marginal effects and plot
library(margins)
avg_marginal_effects <- margins(logit_fit)
summary(avg_marginal_effects)
plot(avg_marginal_effects, main="AME & Confidence Intervals")

# (6) Predict on test set, calculate accuracy etc.
test_prob <- predict(logit_fit, type="response", newdata=test)
test_pred <- test_prob > 0.5
table <- table(Predict=test_pred, Actual=test$chd)
# Fill in metrics
(Accuracy    <- (table[1,1] + table[2,2]) / sum(table))
(Error_rate  <- (table[2,1] + table[1,2]) / sum(table))
(Sensitivity <- table[2,2] / (table[1,2] + table[2,2]))
(Specificity <- table[1,1] / (table[1,1] + table[2,1]))
(Recall      <- table[2,2] / (table[2,1] + table[2,2]))

# (7) Plot ROC curve
install.packages("ROCR")
library(ROCR)
pred_obj <- prediction(test_prob, test$chd)
perf_obj <- performance(pred_obj, measure="tpr", x.measure="fpr")
plot(perf_obj, main="ROC Curve", lwd=2, col="blue", xlab="False Positive Rate", ylab="Sensitivity")

# (8) Calculate AUC
auc_test <- performance(pred_obj, measure='auc')
auc_test@y.values

# (9) Calculate Kappa
library(vcd)
Kappa(table)
