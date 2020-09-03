#Wine example of PCA
wine <- read.csv("//Users//smitshah//Desktop//Assignments//PCA//wine.csv")
View(wine)
str(wine)
help("princomp")
View(wine[-1])

data <- wine[,-1]
attach(data)
cor(data)

#Building PCA summary
pca <- princomp(data,cor = TRUE,scores = TRUE,covmat = NULL)
str(pca)
summary(pca)
plot(pca)
#As per summary ,first 7 variables shows ~90% information required for entire data.
#Comp 1 has high variance
biplot(pca)
#showing increase of varience with considering principal components which help to choose number of principal componets
plot(cumsum(pca$sdev*pca$sdev)*100/(sum(pca$sdev*pca$sdev)),type = 'b')
pca$loadings
pca$scores
pca$scores[,1:3] #Top 3 scores represents whole data

#cbind is used to combine data,consider 3 principle component and combine to the data
data <- cbind(data,pca$scores[,1:3])
View(data)

#Preparing data for clustering
clust_data <- data[,14:16]
#Normalizing data
norm_clust <- scale(clust_data)
dist1 <- dist(norm_clust,method = "euclidean")

#Clustering using hclust function
fit1 <- hclust(dist1,method = "complete")
plot(fit1)
groups <- cutree(fit1,5)
membership_1<-as.matrix(groups)
View(membership_1)
final1 <- cbind(membership_1,data)
View(final1)

#Optimum K Value
library(NbClust)
no_of_Clusters <- NbClust(wine, distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")
fviz_nbclust(no_of_Clusters) + theme_minimal()
#Optimum no of clusters at k=7
#Clustering using h clust
pca$scores[,1:7] #Top 7 variables
#Combine 7 data
data <- cbind(data,pca$scores[,1:7])
View(data)
#Preparing data for clustering
clust_data <- data[,17:23]
#Normalizing data
norm_clust <- scale(clust_data)
dist2 <- dist(norm_clust,method = "euclidean")

#Clustering using hclust function
fit <- hclust(dist2,method = "complete")
plot(fit)
groups <- cutree(fit,7)
membership_1<-as.matrix(groups)
View(membership_1)
final <- cbind(membership_1,data)
View(final)

#K-Means
library(plyr)
head(wine)
str(wine)
summary(wine)
library(corrplot)
corrplot(cor(wine),method = "number",type = "lower")
dim(wine)
library(cluster)
library(plyr)
#Model Building
fit2 <- kmeans(norm_clust,5)
str(fit2)
table(fit2$cluster)
#Final data
final2 <- data.frame(wine,fit2$cluster)
View(final2)
library(data.table)
setcolorder(final2, neworder = c("fit2.cluster"))
View(final2)

