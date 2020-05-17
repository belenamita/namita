
# Salaries
Salary <- read.csv("//Users//smitshah//Desktop//Assignments//Simple linear regressin//Salary_Data.csv")
View(Salary)
attach(Salary)

#Model Building
fit2 <- lm(Salary~YearsExperience)
str(Salary)
