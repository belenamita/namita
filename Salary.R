#Support vector machine
library(e1071)
library(SVMlight)
install.packages("klar")
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(kernlab)


#read data
train <- read.csv("//Users//smitshah//Desktop//Assignments//Support vector machine//SalaryData_Train(1).csv")
test <- read.csv("//Users//smitshah//Desktop//Assignments//Support vector machine//SalaryData_Test(1).csv")
View(train)
str(train)
str(test)
train$educationno <- as.factor(train$educationno)
test$educationno <- as.factor(test$educationno)
help(kvsm)
class(train)

#Data visualization
#Plot and ggplot
ggplot(data = train,aes(x=train$Salary,y=train$age,fill= train$Salary))+geom_boxplot()+ggtitle("Box Plot")
plot(train$workclass,train$Salary)
plot(train$education,train$Salary)
plot(train$educationno,train$Salary)
plot(train$maritalstatus,train$Salary)
plot(train$occupation,train$Salary)
plot(train$relationship,train$Salary)
plot(train$race,train$Salary)
plot(train$sex,train$Salary)

#Building Model
model1 <- ksvm(train$Salary~.,data=train,kernel=("vanilladot"))
model1
pred1 <- predict(model1,newdata= test)
mean(pred1==test$Salary)#0.8464143

#kernel=rbfdot
model_rfdot <- ksvm(train$Salary~.,data= train,kernel="rbfdot")
model_rfdot
pred2 <- predict(model_rfdot,newdata=test)
mean(pred2==test$Salary)#0.8520584

#kernel=besseldot
model_besseldot <- ksvm(train$Salary~.,data= train,kernel="besseldot")
model_besseldot
pred3 <- predict(model_besseldot,newdata=test)
mean(pred3==test$Salary)#0.7897078

#kernel=polydot
model_polydot <- ksvm(train$Salary~.,data=train,kernel="polydot")
model_polydot
pred4 <- predict(model_ploydot,newdata=test)
mean(pred4==test$Salary)
