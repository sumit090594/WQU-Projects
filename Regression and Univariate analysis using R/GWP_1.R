#This project delve into regression and univariate analysis, demonstrating basic understanding of statistics and R code

library(tidyverse)
library(stats)
library(readxl)
library(tseries)
library(forecast)


#Path of excel file should be updated according the user
stock_data <- read_excel("C:/Users/RAWAT/Downloads/GWP_1/combined.xls", sheet = "combined",col_names = T, col_types = c("date", "numeric","numeric"))
#To visualize the stock data of JP Morgan adnd S&P index over the mentioned time period
ggplot(data = stock_data) + geom_line(mapping = aes(x = Date, y = JPM_AdjClose), color = "darkred") + geom_line(mapping = aes(x = stock_data$Date, y = stock_data$`S&P_AdjClose`), color = "blue") + labs(x = "Time Period", y = "Stock Price")

#----Solution 3.1.1
#Average stock value of JP Morgan
mean(stock_data$JPM_AdjClose)

#Daily stock return
stock_return <-diff(stock_data$JPM_AdjClose)/stock_data$JPM_AdjClose[-length(stock_data$JPM_AdjClose)]

#Avg of daily stock return in %
mean(stock_return)*100

#Stock price volatility
sd(stock_data$JPM_AdjClose)

#Stock return volatility
sd(stock_return)

#---Solution 3.1.2
regression = lm(formula = stock_data$JPM_AdjClose~stock_data$`S&P_AdjClose`)
summary(regression)
#Result shows that all coefficient are significant at 5% significance level, P value is acceptable
#adjsuted R square is also good as independent variables are able to explain 57.69% of dependent variables

#---Solution 3.1.3
#Path of excel file should be updated according the user
#We are taking yearly percant change data, so that we can avoid applying log to data
CSUSHPINSA<-read_excel("C:/Users/RAWAT/Downloads/GWP_1/CSUSHPINSA_Percent.xls", sheet = "FRED Graph",col_names = T, col_types = c("date", "numeric"), skip = 10)
ggplot(data = CSUSHPINSA) + geom_line(mapping = aes(x = observation_date, y = CSUSHPINSA_PC1), color = "darkred")

# Convert the data frame to a time series object
NHPI_percent_change <- ts(CSUSHPINSA$CSUSHPINSA_PC1, start=1988, frequency=12)
#Sample for comparing with forecast data
#NHPI_percent_change_OutOfSample <- window(NHPI_percent_change, start = 2018)
#We will build our model from sample data starting from 1988 to end of 2017, rest of data is kept as Out of sample

#---Augmented Dickey-Fuller Test
adf.test(window(NHPI_percent_change,start=1988,end = 2018)) 
#Result shows that there exis unit root in series, due to P-Value at 5% significance level, we can not reject Null hypothesis,
#Hence, series is not stationary. SO Its clear that we have to ARIMA model our series forcasting

#---Implementation of an ARIMA(p,d,q) model using Box-Jenkins methodology.
#We will first analyse ACF and PACF chart of series

acf(window(NHPI_percent_change,start=1988,end = 2018))
pacf(window(NHPI_percent_change,start=1988,end = 2018))
#The ACF of the full-time path of the inflation variable is typical of a non-stationary process: auto-correlations are very close to 1 and slowly fade.
#PACFshowing there has strong impact of the first lag - notice that pacf() by default drops the zero-??????? lag.
#According to pattern of ACF and PACF, its clear that series has AR signature, we can take ARIMA (1,d,0) for our starting point (fitting and evaluating process)



# fit a simple ARIMA model with 1 lags, with one differencing, no moving average terms - i.e. an ARIMA(1,0,0) model:
ARIMA_model1 <- arima(window(NHPI_percent_change,start=1988,end = 2018), order=c(1,1,0), method = "ML")
summary(ARIMA_model1)
#Let's check if model has white noise errors:
Box.test(ARIMA_model1$residuals, lag = 1)

# fit a simple ARIMA model with 1 lags, with one differencing, no moving average terms - i.e. an ARIMA(1,0,0) model:
ARIMA_model2 <- arima(window(NHPI_percent_change,start=1988,end = 2018), order=c(2,1,0), method = "ML")
summary(ARIMA_model2)
#Let's check if model has white noise errors:
Box.test(ARIMA_model2$residuals, lag = 1)

#Forecasting the future evolution of Case-Shiller Index
AR_forecast <- predict(ARIMA_model2, n.ahead= 18, se.fit=TRUE)

#plotting the forecasted data in red line
plot.ts(NHPI_percent_change)
#plot.ts(AR_forecast$pred,col="blue")
lines(AR_forecast$pred,col="red")
lines(AR_forecast$pred + 2*AR_forecast$se,col="red", lty = "dashed")
lines(AR_forecast$pred - 2*AR_forecast$se,col="red", lty = "dashed")
