data(iris)

newiris <- iris
newiris$Species <- NULL

kc <- kmeans(newiris, 3)

str(kc)

table(iris$Species , kc$cluster)

plot(newiris[,c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 2)


library(cluster)
dissE <- daisy(newiris)
sk <- silhouette(kc$cluster, dissE)
plot(sk)

y <- matrix(rnorm(50), 10, 5, dimnames=list(paste("g", 1:10, sep=""), paste("t", 1:5, sep=""))) 

c <- cor(t(y), method="spearman")
d <- as.dist(1-c)


hr <- hclust(d, method="complete", members=NULL)

par(mfrow = c(2,2))
plot(hr, hang = 0.1)
plot(hr, hang = -1)
plot(as.dendrogram(hr), edgePar=list(col=3, lwd=4), horiz=T)

mycl <- cutree(hr, h=max(hr$height)/2)
mycl[hr$labels[hr$order]]
plot(hr)
rect.hclust(hr, k=5, border="red")