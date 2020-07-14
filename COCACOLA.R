
library(readxl)
library(forecast)
library(fpp)
library(smooth)
cocacola <- read_xlsx("//Users//smitshah//Downloads//CocaCola_Sales_Rawdata.xlsx")
View(cocacola)
plot(cocacola$Sales)

#Make dummy variables
Q1 <-  ifelse(grepl("Q1",cocacola$Quarter),'1','0')
Q2 <-  ifelse(grepl("Q2",cocacola$Quarter),'1','0')
Q3 <-  ifelse(grepl("Q3",cocacola$Quarter),'1','0')
Q4 <-  ifelse(grepl("Q4",cocacola$Quarter),'1','0')
cocacola1 <- cbind(cocacola,Q1,Q2,Q3,Q4)
View(cocacola1)
colnames(cocacola)
cocacola1["t"]<-c(1:42)
cocacola1["t_square"] <- cocacola1["t"]*cocacola1["t"]
cocacola1["log_Sales"] <- log(cocacola1["Sales"])
View(cocacola1)
attach(cocacola1)
train <- cocacola1[1:35,]
test <- cocacola1[35:41,]

#Linear model
linear_model <- lm(Sales~t,data=train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model,interval = 'predict',newdata = test))
rmse_mean <- sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_mean

#Exponential model
expo_model <- lm(log_Sales~t,data=train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model,interval = 'predict',newdata = test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo

#Quadratic
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad
  
#Additive seasonality
sea_add_model<-lm(Sales~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add

#Additive seasonality with quadratic
Add_sea_Quad_model<-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad

# Multiplicative seasonality
multi_sea_model<-lm(log_Sales~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea

#Multiplicative Additive Seasonality
multi_add_sea_model<-lm(log_Sales~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea

#Preparing table of models and its rmse values
table_rmse<-data.frame(c("rmse_mean","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_mean,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

#Additive seasonality with qudratic has least rmse value
new_model <- lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data=train)

#Predicting new data

setwd("/Users/smitshah")
write.table(cocacola1,file = "cocacola1.csv",col.names = FALSE,row.names = FALSE)
cocacola2 <- read.csv("cocacola2.csv")
View(cocacola2)

pred_new <- data.frame(predict(new_model,interval = 'predict',newmodel=cocacola2))
pred_new
View(pred_new)
plot(pred_new$fit)

