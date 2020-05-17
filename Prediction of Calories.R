#Question 1
Calories <- read.csv('//Users//smitshah//Desktop//Assignments//Simple linear regressin//calories_consumed.csv')
View(Calories)
attach(Calories)
#Model building
fit <- lm(Weight.gained..grams.~Calories.Consumed)
summary(fit)

#As P value is significant.The gives best fit line with R squared value = 0.8968 

PR1 <- predict(fit)
summary(PR1)
PR1
residuals(fit)
coef(fit)
confint(fit)
