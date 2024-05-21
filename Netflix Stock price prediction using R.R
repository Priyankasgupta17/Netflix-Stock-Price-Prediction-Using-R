library(fpp2)
library(forecast)
library(ggplot2)
library(scales)
data = read.csv("~/Downloads/NFLX.csv")

df = ts(data)

autoplot(df,facets=T)
autoplot(df[,"Close"])
autoplot(df[,"High"])
autoplot(df[,"Low"])
autoplot(df[,"Volume"])
autoplot(df[,"Adj.Close"])

price= df[,'Close']
price
tail(price)
autoplot(price)

ggtsdisplay(price)

lamb = BoxCox.lambda(price)
autoplot(cbind(price,log(price),BoxCox(price,lamb)),facets = T) # boxcox

ndiffs(BoxCox(price,lamb)) # 1
#nsdiffs(BoxCox(price,lamb))  # no seasonality as annual data


library(urca)

ur.df(BoxCox(price,lamb),type = "trend",lags = 22)%>%summary  # not rejected
ur.df(diff(BoxCox(price,lamb)),type = "trend",lags = 22)%>%summary # null rejected

# differencing = 1

recommended_Arima = auto.arima(BoxCox(price,lamb), d=1) 
recommended_Arima #-7017.74
checkresiduals(recommended_Arima)  #White noise, good p value


#modelling
mod1 = Arima(price, order = c(2,1,1),lambda = lamb)
summary(mod1) # 

mod2 = Arima(price, order = c(5,1,1),lambda = lamb)
summary(mod2)  #-16914.24


mod3 = Arima(price, order = c(6,1,1),lambda = lamb)
summary(mod3) # worst


mod4 = Arima(price, order = c(5,1,0),lambda = lamb)  
summary(mod4)  #-16916.25

mod5 = Arima(price, order = c(5,1,2),lambda = lamb)
summary(mod5) #worst

# The best model we got is mod2

checkresiduals(mod4)

#forecasting

## with recommended model


fc1= forecast(recommended_Arima, h = 100)
fc1
autoplot(fc1)  # the auto arima recommended model

fc2= forecast(mod2, h = 100)
fc2  # our model
autoplot(fc2)

ets_suggested_model= ets(price)
ets_suggested_model


fc3= forecast(ets_suggested_model, h = 100)
fc3
autoplot(fc3)

autoplot(window(price,start=2012))+
  autolayer(fc1,series = "ARIMA",PI=F)+  # with recommended auto arima model
  autolayer(fc3,series = "ETS",PI=F)+
  autolayer(fc2,series = "OUR MODEL",PI=F)  # our model



