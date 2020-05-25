computer <- read.csv("//Users//smitshah//Desktop//Assignments//Multi linear  Regression//Computer_Data (1).csv")
View(computer)
attach(computer)
install.packages("dummies")
computer1 <- dummy.data.frame(computer,sep=".")
View(computer1)

summary(computer1)
str(computer1)
computer.new <- computer1[1:6259,c(2:11)]
View(computer.new)

pairs(computer.new)
plot(computer.new)

cor(computer.new)
cor2pcor(cor(computer.new))

#model Building
attach(computer.new)
model1 <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
model1
summary(model1)

predict(model1)
#All depnedant variables are significant with each other.
qqplot(model1,id.n= 5)
