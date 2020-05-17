Delitime <- read.csv("//Users//smitshah//Desktop//Assignments//Simple linear regressin//delivery_time.csv")
View(Delitime)
attach(Delitime)

#Model
Fit1 <- lm(Delivery.Time~Sorting.Time)
Fit1
summary(Fit1)
#Pvalue is significant

Hike <- predict(Fit1)
Hike
