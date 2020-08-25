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



