#Fraud checking
library(randomForest)
library(caret)

#Read data
frd <- read.csv("//Users//smitshah//Desktop//Assignments//Random Forest//Fraud_check.csv")
View(frd)
set.seed(123)
hist(frd$Taxable.Income)
risky_good <- ifelse(frd$Taxable.Income<=30000,"Risky","Good")
frdrg <- data.frame(frd,risky_good)
ftemp <- frdrg[,c(1:7)]
str(ftemp)

#data partition
set.seed(123)
frc <- sample(2, nrow(frdrg), replace = TRUE, prob = c(0.7,0.3))
train <-frdrg[frc==1,]
test <- frdrg[frc==2,]

set.seed(213)
rf <- randomForest(risky_good~.,data = train)
rf
#Predict train data
pred <- predict(rf,train)
pred
head(pred)
head(train$risky_good)
confusionMatrix(pred,train$risky_good)

#Predict the test data
pred1 <- predict(rf,test)
head(pred1)
head(test$risky_good)
confusionMatrix(pred1,test$risky_good)

#Accuracy is 100 %
#Error rate
plot(rf)
#From 200 there is constant line and does not vary after 200
#Tune random forest model mtry
tune <- tuneRF(train[,-6],train[,6],stepFactor = 0.5,plot = TRUE,ntreeTry = 300,trace=TRUE,improve = 0.05)
rf1 <- randomForest(risky_good~.,data = train,ntree=200,mtry=2,importance=TRUE,proximity=TRUE)
rf1
pred2 <- predict(rf1,train)
confusionMatrix(pred2,train$risky_good)
