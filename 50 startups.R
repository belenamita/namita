#Prediction model for profit of 50 startups
startup <- read.csv("//Users//smitshah//Desktop//Assignments//Multi linear  Regression//50_Startups (1).csv")
View(startup)
attach(startup)
summary(startup)

install.packages("dummies")
library(dummies)
startup.new <- dummy.data.frame(startup,sep=".")
names(startup.new)
dummy(startup$State,sep=".")
attach(startup.new)

#Find relation between variables
pairs(startup)
plot(startup)

#Corelation coefficient matrix

cor(startup.new)

#Partial coefficient matrix
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startup.new))

#Model Building
model <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup.new)
summary(model)
model.1 <- lm(Profit~Administration+Marketing.Spend)
summary(model.1)

library(psych)
pairs.panels(startup.new)

#Plotting influential measure
influence.measures(model)
library(car)
influenceIndexPlot(model,id.n=3)
influencePlot(model,id.n=3)

#Regression after deleting 50 th observation
model1 <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup.new[-c(46,47,49,50),])
summary(model1)
vif(model1)
#Vif is not greater than 10 so there in non collinearity among the variables

#Regression after considering R.D.Spend and Marketing.Spend
Finalmodel <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(Finalmodel)

#Evaluate model line assumption
plot(Finalmodel)
#Residual plots,QQplot,std-Residuals Vs Fitted,Cook's Distance 
qqPlot(Finalmodel,id.n = 5)
# QQ plot of studentized residuals helps in identifying outlier 
