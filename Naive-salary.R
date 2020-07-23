#Salary data
salary_test <- read.csv("//Users//smitshah//Desktop//Assignments//Naive Bayes//SalaryData_Test.csv")
salary_train <- read.csv("//Users//smitshah//Desktop//Assignments//Naive Bayes//SalaryData_Train.csv")
install.packages("mlbench")
library(mlbench)
install.packages("e1071")
library(e1071)
str(salary_train)
train_sal_educationno <- as.factor(salary_train$educationno)
class(salary_train)
test_sal_educationno <- as.factor(salary_test$educationno)
#data visualization
library(ggplot2)
ggplot(data=salary_train,aes(x=salary_train$Salary,Y=salary_train$age ,fill=salary_train$Salary))+geom_boxplot()+ggtitle("Box Plot")
plot(salary_train$workclass,salary_train$Salary)
plot(salary_train$education,salary_train$Salary)
plot(salary_train$maritalstatus,salary_train$Salary)
plot(salary_train$occupation,salary_train$Salary)
plot(salary_train$relationship,salary_train$Salary)
plot(salary_train$race,salary_train$Salary)
plot(salary_train$capitalgain,salary_train$Salary)
#Naive bayes model
model_salary <- naiveBayes(salary_train$Salary~.,data = salary_train)
model_salary

#Model Prediction
model_Pred <- predict(model_salary,salary_test)
View(model_Pred)
mean(model_Pred==salary_test$Salary)

library(caret)
confusionMatrix(model_Pred,salary_test$Salary)
