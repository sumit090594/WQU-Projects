#Understanding of volatility analysis by investigating and analyzing a Apple stock return
library(rugarch)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest) 
library(forecast)


apple <- read.table(file='aapl.csv', header=T, sep=",")
apple$Adj.Close	 <- as.numeric(as.character(apple$Adj.Close),fixed=TRUE)
apple <- apple[complete.cases(apple), ]
applets <- zoo(apple$Adj.Close, as.Date(as.character(apple$Date), format = c("%Y-%m-%d")))

# selecting timeframe from 2002 to till now.
applets <- applets[index(applets) > as.Date("2002-01-01 00:00:00.000")]

#log return time series
apple_rets <- log(applets/lag(applets,-1)) 

#strip off the dates and create numeric object
apple_ret_num <- coredata(apple_rets)

#Time series plot 
plot(applets, type='l', xlab = "Time period", ylab = " adj close price", main="Plot of 2002-2019 daily apple stock prices", col = 'red')

#ACF and PACF plot
acf(coredata(applets), main="ACF plot of the 2002-2019 daily apple stock prices")

pacf(coredata(applets), main="PACF plot of the 2002-2019 daily apple stock prices")

basicStats(apple_rets)

#QQ-plot
qqnorm(apple_rets)
qqline(apple_rets, col = 2) 

#Time plot of log return of prices
plot(apple_rets, type='l', ylab = "stock price return", main="Plot of 2002-2019 daily apple stock price return")

#ACF plot of log return of prices
par(mfrow=c(2,1))
acf(apple_ret_num, na.action = na.pass)

#ACF plot of square of log return of prices
acf(apple_ret_num^2, na.action = na.pass)

#Ljung Box test on squared values of the stock price returns
Box.test(apple_ret_num^2, lag=2, type="Ljung")

Box.test(apple_ret_num^2, lag=4, type="Ljung")

Box.test(apple_ret_num^2, lag=6, type="Ljung")

#Ljung Box test on absolute values of the stock price returns
Box.test(abs(apple_ret_num), lag=2, type="Ljung")

Box.test(abs(apple_ret_num), lag=4, type="Ljung")

Box.test(abs(apple_ret_num), lag=6, type="Ljung")

#Determine the order of the model
#PACF plot on the log return of the stock prices
pacf(apple_ret_num, lag=10, na.action = na.pass, main="PACF plot of the log return of the stock prices")

#PACF plot on the squared return of the stock prices
pacf(apple_ret_num^2, na.action = na.pass, lag=10, main="PACF plot of the squared log return of the stock prices")

garch11.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)))
#estimate model 
garch11.fit=ugarchfit(spec=garch11.spec, data=apple_rets)
garch11.fit

#create selection list of plots for garch(1,1) fit
plot(garch11.fit, which = "all")

garch11.t.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")

#estimate model 
garch11.t.fit=ugarchfit(spec=garch11.t.spec, data=apple_rets)
garch11.t.fit

#plot of residuals
plot(garch11.t.fit, which = "all")

garch11.skt.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "sstd")
#estimate model 
garch11.skt.fit=ugarchfit(spec=garch11.skt.spec, data=apple_rets)
garch11.skt.fit

plot(garch11.skt.fit, which = "all")

egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=apple_rets)
egarch11.t.fit

plot(egarch11.t.fit, which = "all")

fgarch11.t.spec=ugarchspec(variance.model=list(model = "fGARCH", garchOrder=c(1,1), submodel = "APARCH"), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
fgarch11.t.fit=ugarchfit(spec=fgarch11.t.spec, data=apple_rets)
fgarch11.t.fit

plot(fgarch11.t.fit, which = "all")

igarch11.t.spec=ugarchspec(variance.model=list(model = "iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0 , 0 )), distribution.model = "std")
#estimate model 
igarch11.t.fit=ugarchfit(spec=igarch11.t.spec, data=apple_rets)
igarch11.t.fit

plot(igarch11.t.fit, which = "all")

## Now lets forecast the t+1 return usign the Egarch model with t-distribution
modelfor=ugarchforecast(egarch11.t.fit, data = applets, n.ahead = 30)
modelfor
plot(modelfor, which = 'all')
