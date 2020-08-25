#Crime data
Crime <- read.csv(file.choose())
View(Crime)
library(cluster)


#Normalisation
normalised_data <- scale(Crime[,2:5])
View(normalised_data)

d <- dist(normalised_data,method = "euclidean")
d

#Model Building
fit <- hclust(d,method = "complete")
fit

plot(fit,hang = -1)
groups <-cutree(fit,k=5,h=NULL)
class(groups)
table(groups)
rect.hclust(fit,k=4,border = "blue")

membership <- as.matrix(groups)
table(membership)

#Final data
final <- data.frame(Crime,membership)
View(final)

#set coloumnin dataframe
library(data.table)
setcolorder(final,c("membership"))
View(final)

#K-means
head(Crime)
str(Crime)
summary(Crime)
library(corrplot)
corrplot(cor(Crime),method = "number",type = "lower")
dim(Crime)
library(cluster)
install.packages("plyr")
library(plyr)
#model building
fit1 <- kmeans(normalised_data,5)
str(fit1)
table(fit1$cluster)

#Final data
final1 <- data.frame(Crime,fit1$cluster)
View(final1)
library(data.table)
setcolorder(final1, neworder = c("fit.cluster"))
View(final1)
aggregate(Crime[1:50,2:5], by=list(fit$cluster), FUN=mean)
