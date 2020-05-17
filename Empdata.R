Empdata <- read.csv("//Users//smitshah//Desktop//Assignments//Simple linear regressin//emp_data.csv")
View(Empdata)
attach(Empdata)

#Model Building
bestfit <- lm(Salary_hike~Churn_out_rate)
bestfit
summary(bestfit)

#P value is significant.The data shows best fitted value as R squared value =0.8312

#Prediction Model
pr1 <- predict(bestfit)
pr1
