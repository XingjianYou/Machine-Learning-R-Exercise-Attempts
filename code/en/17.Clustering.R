# Chapter 17 Assignment
rm(list=ls())

# 17.1 K-Means clustering on iris data
data(iris)
kfit  <- kmeans(iris[,-5], 3, nstart=20)
(table    <- table(Predicted=kfit$cluster, Actual=iris$Species))
(accuracy <- sum(diag(table)) / sum(table))

# 17.2 Clustering on faithful geyser data
data(faithful)
# (1) Scatter plot
plot(faithful, main="Data of Old Faithful Geyser")
par(mar=c(5,5,5,5))
# (2) K=2 clustering, display results
fit <- kmeans(faithful, 2)
plot(faithful, col=fit$cluster, main="Estimated Clusters (K=2)")
# (3) K=3 clustering, display results
fit <- kmeans(faithful, 3)
plot(faithful, col=fit$cluster, main="Estimated Clusters (K=3)")
# (4) Select K via elbow rule
SSE <- numeric(15)
set.seed(1)
for (k in 1:15) {
  fit    <- kmeans(faithful, k, nstart=20)
  SSE[k] <- fit$tot.withinss
}
plot(1:15, SSE, xlab="K", type="b", main="K-means Clustering")
abline(v=which.min(SSE), lty=2)
# (5) Select K via BIC criterion
BIC <- SSE + 2 * log(272) * (1:15)
plot(1:15, BIC, xlab="K", type="b", main="K-means Clustering")
abline(v=which.min(SSE), lty=2)

# 17.3 Hierarchical clustering on faithful data
# (1) Complete linkage dendrogram
hc_complete <- hclust(dist(faithful), method="complete")
par(mar=c(1,5,3,1))
plot(hc_complete, main="Complete Linkage", cex=0.9, xlab="", sub="")
# (2) Average linkage dendrogram
hc_average <- hclust(dist(faithful), method="average")
plot(hc_average, main="Average Linkage", cex=0.9, xlab="", sub="")
# (3) Single linkage dendrogram
hc_single <- hclust(dist(faithful), method="single")
plot(hc_single, main="Single Linkage", cex=0.9, xlab="", sub="")
# (4) Centroid linkage dendrogram
hc_centroid <- hclust(dist(faithful), method="centroid")
plot(hc_centroid, main="Centroid Linkage", cex=0.9, xlab="", sub="")
# (5) Which linkage method is worst?
# Single linkage appears to be the worst
# (6) For complete linkage with K=2, display clustering result as scatter plot
fit <- cutree(hc_complete, k=2)
plot(faithful, col=fit, main="Data of Old Faithful Geyser")
par(mar=c(5,5,5,5))
