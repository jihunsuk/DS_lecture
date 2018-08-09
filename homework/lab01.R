
data <- matrix(rnorm(120, 0 , 10), 20, 6, dimnames=list(paste("g", 1:20, sep=""), paste("t", 1:6, sep="")))

plot(data, main="plot of data")

kc <- kmeans(data, 2)
kc2 <- kmeans(data, 5)

plot(data[,c("t1", "t2")], col = kc$cluster, main="when k=2")
points(kc$centers[,c("t1", "t2")], col = 1:2, pch = 8, cex = 2)

plot(data[,c("t1", "t2")], col = kc2$cluster, main="when k=5")
points(kc2$centers[,c("t1", "t2")], col = 1:5, pch = 8, cex = 2)

c <- cor(t(data), method="spearman")
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