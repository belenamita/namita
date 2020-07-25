#Cloth sales
com <- read.csv("//Users//smitshah//Desktop//Assignments//Random Forest//Company_Data.csv")
View(com)
install.packages("random forest")
library(randomForest)
library(caret)
str(com)

set.seed(123)
hist(com$Sales,main = "Sales",xlim = c(0,20),breaks = c(seq(10,20,30)),col = c("red","blue","violet","yellow"))
#High sales happened between 5 to 10 thousands with high frequency
highsales=ifelse(com$Sales>9,"yes","no")
comd =data.frame(com[2:11],highsales)
table(comd$highsales)

#Test and train data
set.seed(123)
ind <- sample(2,nrow(comd),replace = TRUE,prob = c(0.7,0.3))
train <- comd[ind==1,]
test <- comd[ind==2,]
set.seed(213)
rdf <- randomForest(highsales~.,data = train)
rdf
#Prediction
pred <- predict(rdf,train)
head(pred)
head(train$highsales)
#So Accuracy in 100 %
confusionMatrix(pred,train$highsales)

#Prediction with Test data
pred1 <- predict(rdf,test)
head(pred1)
head(test$highsales)

confusionMatrix(pred1,test$highsales)

#Error rate of rndom forest
plot(rdf)
