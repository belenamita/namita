#50 startups

library(plyr)
stp <- read.csv(file.choose())
View(stp)
str(stp)
attach(stp)
summary(stp)
stp$State <- as.numeric(revalue(stp$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(stp)
#Data visualization
plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(State, Profit)
pairs(stp)
cor(stp)
#Normalize the data
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
stp_norm <- as.data.frame(lapply(stp,FUN= normalize))
summary(stp_norm$Profit)
stp_norm <- stp_norm[,-6]

View(stp_norm)
#Data partition
train <- stp_norm[1:26,]
test <- stp_norm[27:50,]
#Using multilayered feed forward network
#pakage neuralnet
install.packages("neuralnet")#Regression
install.packages("nnet")#Classification
library(neuralnet)
library(nnet)
str(stp_norm)

#Building model
startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = train)
str(startups_model)
plot(startups_model, rep = "best")
summary(startups_model)
#SSE sum of squared errors
#Evaluating model performance
#compute function to generate output for model prepared
set.seed(12323)
model_results <- compute(startups_model,test[1:4])
preduct_profit <- model_results$net.result

#Predicted model Vs Actual Model
cor(preduct_profit,test$Profit)
plot(preduct_profit,test$Profit)

#Denormalize predicted value
str_max <- max(stp$Profit)
str_min <- min(stp$Profit)
unnormalize <- function(x,min,max){
  return((max-min)*x+min)
}
act_profit_pred <- unnormalize(preduct_profit,str_min,str_max)
head(act_profit_pred)

#Improve model performance
Model1 <- neuralnet(Profit~R.D.Spend+Administration+State+Marketing.Spend,data = train,hidden = 2)
plot(Model1,rep = "best")
set.seed(12323)
model_results1 <- compute(Model1,test[1:4])
pred1 <- model_results1$net.result
cor(pred1,test$Profit)
plot(pred1,test$Profit)
