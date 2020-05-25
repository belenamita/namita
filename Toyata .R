#Toyota corolla
Toyata <- read.csv("//Users//smitshah//Desktop//Assignments//Multi linear  Regression//ToyotaCorolla (1).csv")
View(Toyata)

ToyataC<-Toyata[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
attach(ToyataC)
pairs(ToyataC)
plot(ToyataC)

cor(ToyataC)

cor2pcor(ToyataC)
str(ToyataC)
class(ToyataC)

#model Building
attach(ToyataC)
Model <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(Model)

#Final Model
Finalmodel <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(Finalmodel)

#Prediction Model
Prediction1 <- predict(Finalmodel)
Prediction1
