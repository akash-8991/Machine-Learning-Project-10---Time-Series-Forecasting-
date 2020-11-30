setwd("C:/Users/Akash/Desktop/Study/PGP BABI/Course7_Time Series Forecasting/Project 6")
library(forecast)
library(tseries)
library(TSA)
library(ggplot2)

TSdata = gas
class(TSdata)
start(TSdata)
end(TSdata)
frequency(TSdata)
cycle(TSdata)

summary(TSdata)

TSdata.qtr = aggregate(TSdata, nfrequency = 4)
TSdata.yr = aggregate(TSdata, nfrequency = 1)  
plot.ts(TSdata, main="Monthly Australian Gas Production",xlab="time",ylab="Gas Production")
plot.ts(TSdata.qtr, main="Quarterly Australian Gas Production",xlab="time",ylab="Gas Production")
plot.ts(TSdata.yr, main="Yearly Australian Gas Production",xlab="time",ylab="Gas Production")

seasonplot(TSdata,year.labels=TRUE,year.labels.left=TRUE,col=1:40,pch=19,
           main = "Monthly Australian Gas Production",xlab="Time",
           ylab = "Gas Production")

monthplot(TSdata,main="Monthly Australian Gas Production",
          xlab="Time",ylab="Gas Production")

boxplot(TSdata~cycle(TSdata),main="Monthly Australian Gas Production",
        xlab="Time",ylab="Gas Production")

acf(TSdata)

decompgas=decompose(TSdata)
plot(decompgas)
decompgas1=decompose(TSdata, type="multiplicative")
plot(decompgas1)

adf.test(TSdata)

decompgas
Deseason_gas=(decompgas$trend+decompgas$random) 
ts.plot(TSdata,Deseason_gas,col=c("red","blue"),
        main="Gas Production vs Deseasoned Gas Production")

TSdata_new=ts(TSdata,start=c(1970,1),end=c(1995,8),frequency=12)
DataTrain=window(TSdata_new,start=c(1970,1),end=c(1993,12),frequency=12) 
DataTest=window(TSdata_new,start=c(1994,1),frequency=12) 

adf.test(TSdata_new)

diff_TSdata_new=diff(TSdata_new) 
plot(diff_TSdata_new)

adf.test(diff_TSdata_new)

acf(diff_TSdata_new)
pacf(diff_TSdata_new)

TSdat.arima.fit.train=auto.arima(DataTrain, seasonal=TRUE)
TSdat.arima.fit.train

fit=Arima(DataTrain,c(1,1,1),seasonal=list(order=c(0,1,1),period=12))
fit

plot(fit$x,col="blue") 
lines(fit$fitted,col="red",main="Production:Actual vs Forecast")

Box.test(fit$residuals, type = c("Ljung-Box"))

VecA=cbind(fit$fitted,fit$x)
MAPEA_train=mean(abs(VecA[,1]-VecA[,2])/VecA[,1]) 
MAPEA_train

Arimaforecast=forecast(fit,h=20)
VecB=cbind(DataTest,Arimaforecast)
MAPEA_holdout=mean(abs(VecB[,1]-VecB[,2])/VecB[,1]) 
MAPEA_holdout

ts.plot(VecB[,1],VecB[,2],col=c("blue","red"),xlab="year",ylab="production",
        main="Production:Actual vs Forecast")

Final_model=auto.arima(TSdata_new,seasonal=TRUE)
Final_model
Box.test(Final_model$residuals,type=c("Ljung-Box"))

Final_forecast=forecast(Final_model,h=12) 
plot(Final_forecast)
Final_forecast
