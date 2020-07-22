#Zoo problem
zoo <- read.csv("//Users//smitshah//Desktop//Assignments//KNN//Zoo.csv")
View(zoo)
zoo <- zoo[-1]
summary(zoo[c("feathers","hair","predator")])
col_names <- names(zoo)
zoo[,col_names] <- lapply(zoo[,col_names],factor)
str(zoo)

#Splitting dataset
train <- zoo[1:71,]
test <- zoo[72:101,]
#Data visualization
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
corrplot(cor(zoo))

#KNN Model
library(class)
zoo_pred <- knn(train[1:16],test[1:16],train$type,k=1)
summary(zoo_pred)
View(zoo_pred)
##evaluating model performance
error <- mean(zoo_pred!=test$type)
str(zoo_pred)
str(test)

confusionMatrix(zoo_pred,test$type)
#Confusion matrix shows model has accuracy 90%
zoo_pred <- NULL
error <- NULL
for( i in 1:16) 
{
 zoo_pred <- knn(train[1:16],test[1:16],train$type,k= i)
  error[i] <- mean(zoo_pred!=test$Type)
  
}
knn.error <- as.data.frame(cbind(k=1:17,error.type=error))

ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:17)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
