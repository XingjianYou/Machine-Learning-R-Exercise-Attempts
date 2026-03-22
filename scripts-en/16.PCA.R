# Chapter 16 Assignment
rm(list=ls())

# 16.1 Dimensionality reduction on spam data using PCA
# (1) Standardize spam features then perform PCA
library(ElemStatLearn)
data(spam)
spam <- spam[,-58]
fit  <- prcomp(spam, scale=TRUE)
# (2) Report variance decomposition, how many PCs explain 80%
summary(fit)
# (3) Plot scree plot
pve <- summary(fit)$importance[2,]  # proportion of variance explained
plot(1:57, pve, type="b", main="PVE",
     xlab="Principal Component", ylab="Proportion of Variance Explained")
# (4) Plot cumulative PVE
plot(1:57, cumsum(pve), type="b", main="Cumulative PVE",
     xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained")
abline(h=0.8, lty=2)
# (5) Plot biplot
sample_idx <- sample(4601, 400)
biplot(fit, cex=0.8, col=c(1:4))

fit_sample <- prcomp(spam[sample_idx,], scale=TRUE)
biplot(fit_sample, cex=0.8, col=c(1:4))

# 16.2 PCA on abalone data
rm(list=ls())
# (1) Remove response variable Rings and factor variable Type, then perform PCA
library(AppliedPredictiveModeling)
data(abalone)
abalone <- abalone[, -grep("Rings|Type", colnames(abalone))]
fit_scaled   <- prcomp(abalone, scale=TRUE)
fit_unscaled <- prcomp(abalone)
# (2) Report variance decomposition, how many PCs needed to explain 90%
summary(fit_scaled)
# (3) Interpret each PC from its loading vector
fit_scaled
fit_unscaled
