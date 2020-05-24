#Preamble
setwd("D:/R working dir")
y=function(){dev.new();x=par(no.readonly=T);dev.off();x}
par(y());options(scipen=0);dev.off();rm(list=ls())

#Packages
install.packages("tidyverse")
install.packages("rstudioapi")
install.packages("tsibble")
install.packages("fable")
install.packages("feasts")
install.packages("urca")
install.packages("tseries")
library("rstudioapi")
library("tidyverse")
library("tsibble")
library("lubridate")
library("fable")
library("feasts")
library("urca")
library("tseries") #for DF unit root test


#Data
data <- read_csv("data_sheet1.csv")

#Create a tsibble obj 
data <- mutate(data, date = yearmonth(dmy(date))) %>% 
  tsibble(index = date)

#Plot
data %>%
  ggplot() +
  aes(x = date, y = CPI) +
  geom_line(col = "#FC4E07", size = 1) +
  xlab("Date") + ylab("CPI") + ggtitle("CPI gross") +
  geom_smooth(method = "lm", size = 1)  #trend line shows that we have a trend

# create a TS obj (vector)
DATA <- ts(data[,2], start=1960, frequency = 12) 

#Remove the trend
#We have a stochastic trend, to remove it we need to do differencing
data <- data %>%
  mutate(
    log_CPI = log(CPI),
    d1log_CPI = difference(log(CPI), 1)*100, 
    d12log_CPI = difference(log(CPI), 12)*100
)

#Plot the differenced CPI
data %>%
  ggplot() +
  geom_line(aes(date, log_CPI), col = "#FF0000", size = 0.2) +
  geom_line(aes(date, d1log_CPI), col = "#0000FF", size = 0.2) +
  geom_line(aes(date, d12log_CPI), col = "#00FF00", size = 0.2) +
  xlab("Date") + ylab("log_CPI") + 
  ggtitle("CPI logged") + labs(subtitle = "log(red) log1(blue) log12(green)")  

#Remove the trend component (alternative approach)
decompDATA = decompose(log(DATA))
plot(decompDATA) #all data
plot(decompDATA$trend) #show the trend
plot(decompDATA$seasonal + decompDATA$random) #data without trend component

#DF test for unit roots, 1st lib
#Test in log-levels with (deterministic???) trend
summary(ur.df(data$log_CPI, selectlags = "BIC", type ="trend"))

#DF test for unit roots, 2nd lib
adf.test(log(DATA)) #p-value = 0.9696, fail to reject 0-h, unit root exists

#Estimate AR(1)
ar1 <- data %>%
  model(ar1 = ARIMA(d12log_CPI ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)))

report(ar1) #coef/std.err = 0,9847/0,0061 = 161,42
glance(ar1) #a single row summary

#Use PACF to decide which lag to use in models
pacf <- data %>% 
  PACF(d12log_CPI, lag_max = 10) %>%
  autoplot() +
  xlab("Lags") +
  ylab("PACF")
pacf #2nd and 3rd lags are significant

#Shape of ACF helps to define the lag
acf <- data %>% 
  ACF(d12log_CPI, lag_max = 10) %>%
  autoplot() +
  xlab("Lags") +
  ylab("ACF")
acf 
#the autocorrelation decreases as the lag increases, 
#confirming that there is no linear association between 
#observations separated by larger lags

#Test for residual autocorrelation
LBtest <- ar1 %>%
  residuals() %>% 
  features(features = ljung_box, lag = 10)

#Estimate ARIMA(p,d,q) models to find the less complex one 
#using the information about which lag to use from PACF plot
arma <- data %>%
  model(
    arma00 = ARIMA(d12log_CPI ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(d12log_CPI ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(d12log_CPI ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(d12log_CPI ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(d12log_CPI ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(d12log_CPI ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(d12log_CPI ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(d12log_CPI ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(d12log_CPI ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(d12log_CPI ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(d12log_CPI ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(d12log_CPI ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(d12log_CPI ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(d12log_CPI ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(d12log_CPI ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(d12log_CPI ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0))
  )

glance(arma)
glance(arma)[which.min(glance(arma)[["AIC"]]), ]
glance(arma)[which.min(glance(arma)[["BIC"]]), ]

#Test for ARMA's residual autocorrelation
LBtest <- arma03 %>%
  residuals() %>% 
  features(features = ljung_box, lag = 10)

#Test for normality
JBtest <- arma %>%
  select(arma11) %>%
  as.ts() %>%
  jarque.bera.test()

