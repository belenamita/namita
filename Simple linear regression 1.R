#Calories consumed

Cal=read.csv("//Users//smitshah//Desktop//Assignments//Simple linear regressin//calories_consumed.csv")
library(readr)
summary(Cal)
#Scatter plot
plot(Cal$Weight.gained..grams.,Cal$Calories.Consumed)
attach(Cal)
#corelation coeficient 
cor(Weight.gained..grams.,Calories.Consumed)
#Simple regression Model
reg <- lm(Calories.Consumed~Weight.gained..grams.)
reg
summary(reg)

predict(reg)

reg$residuals
sum(reg$residuals)
mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(Cal))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict(reg,interval="predict")
#ggplot
library(ggplot2)
ggplot2?
 ggplot(data = Cal,aes(x = Weight.gained..grams., y = Calories.Consumed)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = Cal, aes(x=Weight.gained..grams., y=pred)

            