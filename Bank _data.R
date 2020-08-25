#Bank data
bank <- read.csv("//Users//smitshah//Desktop//Assignments//Logistic Regression//bank-full.csv",sep = ";")
View(bank)
attach(bank)
str(bank)
colnames(bank)
class(bank)

#Linear regression
model <- lm(y~.,data = bank)
pred1 <- predict(model,bank)

plot(age,pred1)
plot(pred1)
#Using glm function
model1 <- glm(y~.,data = bank,family = "binomial")
exp(coef(model1))

#confusion matrix table
prob <- predict(model1,bank,type = "response")
summary(prob)
summary(model1)
confusion <- table(prob>0.5,bank$y)
confusion

#model accuracy
accuracy <- sum(diag(confusion)/sum(confusion))
accuracy
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

bank[,"prob"] <- prob
bank[,"pred_values"] <- pred_values
bank[,"yes_no"] <- yes_no
View(bank)

table(bank$y,bank$pred_values)

#ROCR  curve
library(ROCR)
install.packages("ggplot2")
library(ggplot2)

rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

#Sensitivity
sensitivity(bank$y,)
#