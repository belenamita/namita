#Decision tree- company data
install.packages("tree")
install.packages("C50")
library(tree)
library(C50)

com <- read.csv(file.choose())
View(com)

#Data partition
hist(com$Sales)
#High sales seen between 5 to 10 thousands range
hs <- ifelse(com$Sales<10,"NO","YES")
cd <- data.frame(com,hs)
View(cd)
cd1 <- cd[,-1]
train <- cd1[1:200,]
test <- cd1[201:400,]

#Build tree model
cd5.0_train <- C5.0(train[,-11],train$hs)
plot(cd5.0_train)

pred1 <- predict(cd5.0_train,train)

mean(train$hs==pred1)
#98% accuracy

confusionMatrix(pred1,train$hs)
CrossTable(train$hs,pred1)

#Predicting test data
pred2 <- predict(cd5.0_train,test)
mean(test$hs==pred2)
confusionMatrix(pred2,test$hs)
CrossTable(test$hs,pred2)


###using Tree function
library(gmodels)
com_tree <- tree(hs~.,data=train)
summary(com_tree)
plot(com_tree)
text(com_tree,pretty = 0)

#Prediction of test data
pred.t <- data.frame(predict(com_tree,newdata = test))
pred.t["Final"] <- NULL
pred.t_df <- predict(com_tree,newdata = test)
pred.t$final <- colnames(pred.t_df)[apply(pred.t_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)