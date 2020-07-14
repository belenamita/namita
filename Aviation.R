#Aviation
library(readxl)
library(forecast)
library(fpp)
library(smooth)

Aviation <- read_xlsx("//Users//smitshah//Desktop//Assignments//Foresting//Airlines+Data.xlsx")
View(Aviation)
plot(Aviation$Passengers...000.,type = "o")


# make 12 dummy variables
library(dummies)
X <- data.frame(outer(rep(month.abb,length=96),month.abb,"==") + 0)
View(X)
colnames(X) <- month.abb
View(X)
av <- cbind(Aviation,X)
View(av)
av["t"] <- 1:96
View(av)
av["t_square"] <- av["t"]*av["t"]
av["log_Passengers"] <- log(av["Passengers"])
attach(av)
train <- av[1:90,]
test <- av[91:96,]

#Linear model
linear_model <- lm(Passengers~t,data=train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model,interval = 'predict',newdata =test))
View(linear_pred)
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear

#Exponential model
expo_model <- lm(log_Passengers~t,data=train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model,interval = 'predict',newdata = test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

#Quadratic
Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad

#Additive seasonality
sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

#Additive seasonality with quadratic
Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

# Multiplicative seasonality
multi_sea_model<-lm(log_Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

#Multiplicative Additive Seasonality
multi_add_sea_model<-lm(log_Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#Preparing table of models and its rmse values
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Additive seasonality with qudratic has least rmse value
new_model <- lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)

#Predicting new data

setwd("/Users/smitshah")
write.table(Aviation,file = "AV1.csv",col.names = FALSE,row.names = FALSE, sep = ",")
AV1 <- read.csv("AV1.csv")
View(AV1)

pred_new <- data.frame(predict(new_model,interval = 'predict',newmodel=AV1))
pred_new
View(pred_new)
plot(pred_new$fit)

