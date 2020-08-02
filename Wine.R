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
