#Crime data
Crime <- read_csv("//Users//smitshah//Downloads//crime_data.csv")
View(Crime)

#Normalisation
normalised_data <- scale(Crime[,2:5])
View(normalised_data)

d <- dist(normalised_data,method = "euclidean")
d

#Model Building
fit <- hclust(d,method = "complete")
fit
plot(fit)
plot(fit,hang = 1)
groups <- cutree(fit,k=5)
class(groups)
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
