#Glass KNN Model
glass <- read.csv("//Users//smitshah//Downloads//glass.csv")
View(glass)
#Table of RI
table(glass$Type)

#Proportion table % of class attribute
round(prop.table(table(glass$Type))*100)
summary(glass[c("RI","Na","Mg")])

#Normalize data
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Test normalized data
norm(c(100,200,300,400,500))

# Normalize given data
glass_n <- as.data.frame(lapply(glass[1:9], norm))
View(glass_n)
glass1 <- cbind(glass_n,glass[10])

#create train and test datasets
install.packages("caret")
library(caret)
set.seed(123)
trainIndex <- createDataPartition(glass1$Type,p=0.7,list = FALSE,times = 1)
  train <- glass1[trainIndex,]
  test <- glass1[-trainIndex,]

#Data visualization
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
corrplot(cor(glass1))

#Building KNN model on training data
library(class)
glass_test_pred <- knn(train[1:9],test[1:9],train$Type,k=1)
summary(glass_test_pred)
View(glass_test_pred)

#Evaluating model performance
error <- mean(glass_test_pred!=test$Type)

str(glass_test_pred)
str(test$Type)
test_glass1 <-factor(test$Type)
str(test_glass1)
View(test_glass1)
View(glass_test_pred)

CrossTable(glass_test_pred,test_glass1)
confusionMatrix(glass_test_pred,test_glass1)

#The confusion matrix shows model has accuracy around 77.78%.To increase accuracy use different K value
glass_test_pred <- NULL
error <- NULL
for( i in 1:10) 
{
  glass_test_pred <- knn(train[1:9],test[1:9],train$Type,k= i)
  error[i] <- mean(glass_test_pred!=test$Type)
  
}
knn.error <- as.data.frame(cbind(k=1:10,error.type=error))


#Choosing K value from graph
install.packages("ggplot2")
library(ggplot2)
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
#From graph the predicted model has optimum value K=1

