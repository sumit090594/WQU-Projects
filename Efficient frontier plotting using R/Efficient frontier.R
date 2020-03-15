#To read Documents from Excel file 
library(readxl)
etfs<-read_excel("ETFs.xlsx")

#Creating return  and Beta matrix from question 1
rf<-2.25  #Risk Free Return
Beta<-matrix(c(1.07,1.06), nrow = 1, ncol = 2, dimnames = list("Beta", c("XLE","XLI")))
Return<-matrix(c(9.47, 9.41), nrow = 1, ncol = 2, dimnames = list("CAPM Return", c("XLE","XLI")))

#Loading Performance Analytics library for time series data
library(PerformanceAnalytics)
#Converting excel data in time series (XTS format) data and then calculating returns on it
t_etfs<-as.xts(etfs[-1], order.by = as.Date(etfs$Date, format = "%Y/%m/%d" ))
t_etfs_return<-Return.calculate(t_etfs)
t_etfs_return<-t_etfs_return[(-1),] #To remove 1st row

#To calculating Standard Deviation
Stdev<-StdDev.annualized(t_etfs_return, scale = 252)*100
SPReturn<-Return.annualized(t_etfs_return$`S&P`,scale = 252)*100

#Creating matrix of weight for 11 portfolio
weight<-matrix(c(1,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1, 0, 0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), nrow = 11, ncol = 2, dimnames = list( c("Portfolio 1","Portfolio 2", "Portfolio 3", "Portfolio 4","Portfolio 5","Portfolio 6","Portfolio 7","Portfolio 8","Portfolio 9","Portfolio 10","Portfolio 11" ), c("XLE","XLI")))

#Calculating correlation between renturns of XLE and XLI
rho<-cor(t_etfs_return$XLE, t_etfs_return$XLI)

#Calculating protfolio expected returns and portfolio expected volatility for 11 portfoloio
Portfolio_returns<-(weight[,1]*Return[,"XLE"]+weight[,2]*Return[,"XLI"])
Portfolio_Volatility<-(sqrt((weight[,1]*Stdev[,"XLE"])^2+(weight[,2]*Stdev[,"XLI"])^2+2*weight[,1]*Stdev[,"XLE"]*weight[,2]*Stdev[,"XLI"]*rho[1,1]))

#Combining expected volatiility and returns into one table and then draw it scatter plot 
Portfolio<-cbind(Portfolio_Volatility,Portfolio_returns)
plot(Portfolio, col= "red", pch = 20, main = "Efficient Frontier", xlab = "Portfolio Volatility (%)", ylab = "Portfolio Returns (%)")


#Portfoloio 7 is satisfying both condition
#Calculating Daily Active returns for Portfolio 7
AR_P7<-t_etfs_return$XLE*0.4+t_etfs_return$XLI*0.6 - t_etfs_return$`S&P`
colnames(AR_P7)<-"Protfolio 7"

#Risk Adjusted Return
#Tracking error 
TEP7<-StdDev.annualized(AR_P7, scale = 252)*100

#Mean Adjusted Tracking Error
MATEP7<-sqrt((251/252)*(TEP7/100)^2+Return.annualized(AR_P7, scale = 252)^2)*100
row.names(MATEP7)<-"MATE"

#Sharpe Ratio
Sharpe<-(Portfolio_returns[7]-rf)/Portfolio_Volatility[7]
Sharpe
