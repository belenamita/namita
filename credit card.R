#Logistic Regression
#Credit Card Problem

Crcard <- read.csv("//Users//smitshah//Desktop//Assignments//Logistic Regression//creditcard.csv")
attach(Crcard)
str(Crcard)
Crcard.omit=na.omit(Crcard)
Crcard.omit


#Model Building
Model1 <- glm(factor(card)~reports+age+income+share+expenditure+factor(owner)+factor(selfemp)+dependents+months+majorcards+active,family = "binomial",data = Crcard)
summary(Model1)

#Linear regression technique applied
exp(coef(Model1))
table(Crcard$card)

prob1 <- predict(Model1,type = "response",Crcard)
prob1
