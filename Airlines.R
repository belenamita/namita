#East west airlines
install.packages("plyr")
library(plyr)


mydata <- read.csv("//Users//smitshah//Downloads//EastWestAirlines (1) (1).csv")

str(mydata)
normalized_data<-scale(mydata[,2:12])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      # Determine number of clusters by scree-plot 
for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")
fit <- kmeans(normalized_data, 3) # 3 cluster solution
final2<- data.frame(mydata, fit$cluster) # append cluster membership
# final2
aggregate(mydata[,2:12], by=list(fit$cluster), FUN=mean)
table(fit$cluster)

#hierachical clustering
d <- dist(normalized_data,method = "euclidean")
d
fit1 <- hclust(d,method = "complete")
fit1
plot(fit1,hang=-1)
groups <- cutree(fit1,k=44,h=NULL)
table(groups)

View(groups)
airline <- as.matrix(groups)
final <- data.frame(mydata,airline)
View(final)

d1 <- dist(normalized_data,method = "maximum")
d1
fit2 <- hclust(d1,method = "average")
fit2
plot(fit2,hang=-1)
groups1 <- cutree(fit2,k=44,h=NULL)
table(groups1)
View(groups1)
airline11 <- as.matrix(groups)
final1 <- data.frame(mydata,airline)
View(final1)

d2 <- dist(normalized_data,method = "manhattan")
d2
fit3 <- hclust(d2,method = "single")
fit3
plot(fit3,hang=-1)
groups2 <- cutree(fit3,k=44,h=NULL)
table(groups2)
View(groups2)
airline11 <- as.matrix(groups2)
final2 <- data.frame(mydata,airline)
View(final2)

d3 <- dist(normalized_data,method = "euclidean")
d3
fit4 <- hclust(d3,method = "single")
fit4
plot(fit4,hang=-1)
group3 <- cutree(fit4,k=44,h=NULL)
table(group3)

d4 <- dist(normalized_data,method = "maximum")
d4
fit5 <- hclust(d4,method="complete")
fit5
plot(fit5,hang=-1)
group4 <- cutree(fit5,k=44,h=NULL)
table(group4)
