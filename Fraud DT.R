#Fraud check
fc <- read.csv(file.choose())
View(fc)
library(C50)
library(tree)

hist(fc$Taxable.Income)
risky_good <- ifelse(fc$Taxable.Income<=30000,"Risky","Good")
risky_good
fcd <- data.frame(fc[1:6],risky_good)
View(fcd)
#Data splitting
train <- fcd[1:300,]
test <- fcd[301:600,]

#Building model on training model
fcdC5.0 <- C5.0(train[,-7],train$risky_good)
plot(fcdC5.0)

#Training accuracy
pred <- predict(fcdC5.0,train)
mean(train$risky_good==pred)

library(caret)
confusionMatrix(pred,train$risky_good)
#Accuracy is 100%
#resting accuracy
pred1 <- predict(fcdC5.0,test)
mean(test$risky_good==pred1)


###Using tree function
#Building model on train data
fd_tree <- tree(risky_good~.,data = train)
plot(fd_tree)
text(fd_tree,pretty = 0)

#Using Party function
install.packages("png")
install.packages("knitr")
library(knitr)
library(gmodels)
library(png)
install.packages("party")
library(party)
png(filename = "decision_tree.png")
opall_tree=ctree(risky_good~.,data = fcd)
summary(opall_tree)
plot(opall_tree)
#Building model
png(file = "decision_tree.png")
op_tree <- ctree(risky_good~.,data = train)
summary(op_tree)
plot(op_tree)

pred_tree <- as.data.frame(predict(op_tree,newdata=test))
pred_tree["final"] <- NULL
pred_test <- predict(op_tree,newdata=test)

mean(pred_test==test$risky_good)
confusionMatrix(test$risky_good,pred_test)
