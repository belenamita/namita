#Credit card data
credit <- read.csv(file.choose())
str(credit)
class(credit)
summary(credit)
attach(credit)

colnames(credit)
sum(is.na(credit))
View(credit$card)
credit$card <- ifelse(credit$card=="yes",1,0)

#Linear regression
model1 <- lm(card~.,data = credit)
model1
summary(model1)
pred1 <- predict(model1)
plot(credit$card,pred1)
#Linear model can not be used

str(credit)
head(credit)

#Split data
install.packages("caTools")
library(caTools)
split <- sample.split(credit$card,SplitRatio = 0.9)
train <- subset(credit,split == TRUE)
test <- subset(credit,split == FALSE)
nrow(train)
nrow(test)


str(credit)
View(credit)
#Using  glm model
model2 <- glm(card ~ age+income+share+expenditure+months+majorcards+active,family = "binomial",data = train ,maxit=100)
summary(model2)
View(credit)
pred <- predict(model2,type = "response")
pred
summary(pred)
exp(coef(model2))

# Confusion matrix table 
prob <- predict(model2,credit,type="response")
summary(model2)    
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,credit$card)
confusion    
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 
# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
credit[,"prob"] <- prob
credit[,"pred_values"] <- pred_values
credit[,"yes_no"] <- yes_no
View(credit)

# precision | recall | True Positive Rate | False Positive Rate | Specificity | Sensitivity
# from the above table - 59


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
library(ROCR)
rocrpred<-prediction(prob,credit$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)
