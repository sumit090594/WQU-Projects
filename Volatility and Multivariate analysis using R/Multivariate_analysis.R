# load packages and data 
library(readxl) 
library(vars) 
library(timeSeries) 
library(tidyverse)
library(stats)
library(readxl)
library(tseries)
library(forecast)
fx_rate <- read_excel("F:/Msc/DEXUSUK.xls", sheet = "FRED Graph",col_names = T,col_types = c("date", "numeric", "numeric", "numeric"),skip = 10) 
fx_rate$CPIUS
ggplot(data = fx_rate, mapping = aes(x=observation_date,y=DEXUSUK))+ geom_line(color ="darkblue") + labs(x="",y="USD/GBP exchange Rate")
ggplot(data = fx_rate, mapping = aes(x=observation_date,y=CPIUS))+ geom_line(color ="darkblue") + labs(x="",y="CPI US")
ggplot(data = fx_rate, mapping = aes(x=observation_date,y=CPIUK))+ geom_line(color ="darkblue") + labs(x="",y="CPI UK")

# Differencing the data to level1
D_US_UK_Intreset_rate <- diff(fx_rate$DEXUSUK,trim=TRUE) 
D_US_CPI <- diff(fx_rate$CPIUS,trim=TRUE) 
D_UK_CPI <- diff(fx_rate$CPIUK,trim=TRUE)

D_Equilibrum_fx1 = cbind(D_US_UK_Intreset_rate, D_US_CPI, D_UK_CPI)
complete.cases(Equilibrum_fx1)
D_Equilibrum_fx <- na.omit(D_Equilibrum_fx1)
#computing correlation
cor(D_Equilibrum_fx)

#Estimate a Vector Auto Regression

VAR_model <- VAR(D_Equilibrum_fx, lag.max=2, type = "none", ic = "AIC")
summary(VAR_model)

US_UK_Intreset_rate <- fx_rate$DEXUSUK
US_CPI <- fx_rate$CPIUS
UK_CPI <- fx_rate$CPIUK

Equilibrum_fx = cbind(US_UK_Intreset_rate, US_CPI, UK_CPI)
jotest1=ca.jo(Equilibrum_fx, type="eigen", K=2, ecdet="none", spec="longrun") 
summary(jotest1) 
jotest2=ca.jo(Equilibrum_fx, type="trace", K=2, ecdet="none", spec="longrun") 
summary(jotest2)

# Fit cointegrated VECM 
VECM_fit = VECM(Equilibrum_fx,1,r = 2, include = "const",estim = "ML", LRinclude = "none") 
summary(VECM_fit)


