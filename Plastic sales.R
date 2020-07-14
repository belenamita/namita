library(forecast)
library(fpp)
library(smooth)
library(tseries)
#Load data of plastic sales
pls <- read_csv("//Users//smitshah//Desktop//Assignments//Foresting//PlasticSales.csv")
View(pls)
class(pls)
#Converting dataframe into timeseries
amts <- ts(pls$Sales,frequency = 12,start = c(120))
View(amts)
class(amts)
train <- amts[1:56]
test <- amts[57:60]
train <- ts(train,frequency = 12)
test <- ts(test,frequency = 12)
plot(train)

#holtwinters function
#Simple exponential smoothning
#Alpha= 0.2
hw_p <- HoltWinters(train,alpha = 0.2,beta = F,gamma = F)
hw_p
hwa_pred <- forecast(hw_p)
hwa_pred <- data.frame(predict(hw_p,n.ahead=4))
plot(forecast(hw_p,h=4))
hwa_mape<-MAPE(hwa_pred$fit,test)*100
hwa_mape
#With alpha=0.2 ,beta= 0.1
hw_ab <- HoltWinters(train,alpha=0.2,beta=0.1,gamma=F)
hw_ab
hwab_pred <- data.frame(predict(hw_ab,n.ahead=4))
plot(forecast(hw_ab,h=4))
hwab_mape <- MAPE(hwab_pred$fit,test)*100
hwab_mape
#with alpha = 0.2, beta= 0.1, gamma= 0.1
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg
hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 4))
plot(forecast(hw_abg,h=4))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
hwabg_mape

#Without optimum values
hw_na<-HoltWinters(train,beta = F,gamma = F)
hw_na
hwna_pred<-data.frame(predict(hw_na,n.ahead = 4))
hwna_pred
plot(forecast(hw_na,h=4))
hwna_mape<-MAPE(hwna_pred$fit,test)*100
hwna_mape

hw_nab<-HoltWinters(train,gamma=F)
hw_nab
hwnab_pred<-data.frame(predict(hw_nab,n.ahead=4))
hwnab_pred
plot(forecast(hw_nab,h=4))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100
hwnab_mape

hw_nabg<-HoltWinters(train)
hw_nabg
hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =4))
hwnabg_pred
plot(forecast(hw_nabg,h=4))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
hwnabg_mape

#creating dataframe and build and find MAPE value
df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)


##Using ses,Holt,hw functions
#With optimum values
#alpha=0.2

#Simple exponential method
ses_p <- ses(train,alpha = 0.2)
ses_p
sesp_pred <- data.frame(predict(ses_p,h=4))
sesp_pred
plot(forecast(ses_p,n.ahead=4))
sesp_mape <- MAPE(sesp_pred$Point.Forecast,test)*100
sesp_mape

#With alpha=0.2,beta=0.1
holt_ab<-holt(train,alpha = 0.2,beta = 0.1)
holt_ab
holtab_pred<-data.frame(predict(holt_ab,h=4))
plot(forecast(holt_ab,h=4))
holtab_mape<-MAPE(holtab_pred$Point.Forecast,test)*100
holtab_mape

# with alpha = 0.2, beta = 0.1, gamma = 0.1 

hw_abg_new<-hw(train,alpha = 0.2,beta = 0.1,gamma = 0.1)
hw_abg_new
hwabg_pred_new<-data.frame(predict(hw_abg_new,h = 4))
plot(forecast(hw_abg_new,h=4))
hwabg_mape_new<-MAPE(hwabg_pred_new$Point.Forecast,test)*100
hwabg_mape_new

#Without optimum values
#Simple Exponential method
ses_na<-ses(train,alpha=NULL)
ses_na
sesna_pred<-data.frame(predict(ses_na,h = 4))
sesna_pred
plot(forecast(ses_na,h=4))
sesna_mape<-MAPE(sesna_pred$Point.Forecast,test)*100
sesna_mape

#Holts winter method
holt_nab<-holt(train,alpha = NULL,beta = NULL)
holt_nab
holtnab_pred<-data.frame(predict(holt_nab,h=4))
holtnab_pred
plot(forecast(holt_nab,h=4))
holtnab_mape<-MAPE(holtnab_pred$Point.Forecast,test)*100
holtnab_mape

# Holts winter Exponential method

hw_nabg_new<-hw(train,alpha=NULL,beta=NULL,gamma = NULL)
hw_nabg_new
hwnabg_pred_new<-data.frame(predict(hw_nabg_new,h=4))
hwnabg_pred_new
plot(forecast(hw_nabg_new,h=4))
hwnabg_mape_new<-MAPE(hwnabg_pred_new$Point.Forecast,test)*100
hwnabg_mape_new

#Dataframe and respective MAPE
df_mapes_new<-data.frame(c("sesp_mape","holtnab_mape","hwabg_mape_new","sesna_mape","holtnab_mape","hwnabg_mape_new"),c(sesp_mape,holtnab_mape,hwnabg_mape_new,sesna_mape,holtnab_mape,hwnabg_mape_new))
colnames(df_mapes_new)<-c("MAPE","VALUE")
View(df_mapes_new)

#Final model
final_model <- HoltWinters(amts)
plot(forecast(final_model))
final_pred <- data.frame(predict(amts,h=4))
final_new <- MAPE(final_pred$Point.Forecast,test)*100
final_new

#Moving average
ma_model <- sma(train)
ma_pred <- data.frame(predict(ma_model,h=4))
ma_pred
plot(forecast(ma_model))
ma_mape<-MAPE(ma_pred$Point.Forecast,test)*100
ma_mape

#ARIMA Model
plot(train)
acf(train)
pacf(train)
a <- arima(train,order = c(1,1,8),method = "ML")

#Auto.Arima model on the price agg data
library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))

acf(model_AA$residuals)
pacf(model_AA$residuals)
plot(forecast(model_AA,h=12),xaxt="n")
