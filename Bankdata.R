bankdata <- read.csv("//Users//smitshah//Desktop//Assignments//Logistic Regression//bank-full.csv",sep = ";")
View(bankdata)
str(bankdata)

library(caTools)
split <- sample.split(bankdata,SplitRatio =0.8)
split
training <- subset(bankdata,split=="TRUE")
testing <- subset(bankdata,split=="FALSE")

model <- glm(y~.,training,family = "binomial")
summary(model)
 Model1 <- glm(y~.-pdays,training,family = "binomial")
Model1 
summary(Model1)

res <- predict(Model1,training,type="response")
res

table(ActualValue=testing$y,PredictedValue=res>0.5)

accuracy <- (9126+433)/(9126+839+433+238)
accuracy

#ROC curve
library(ROCR)
ROCRpred <-  prediction(res,training$y)
ROCRpref <- performance(ROCRpred,"tpr","fpr")
plot(ROCRpref,colorize=TRUE,print.cutoff.at=seq(0.1,by=0.1))
     