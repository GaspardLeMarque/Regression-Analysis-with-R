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
library("tseries") 


#Read the dataset
data <- read_csv("data_sheet1.csv")

#Create a tsibble obj 
data <- mutate(data, date = yearmonth(dmy(date))) %>% 
  tsibble(index = date)
#Create a TS obj (vector)
DATA <- ts(data[,2], start=1960, frequency = 12) 

# -----------------------------------------------------------------------------

#Plot the TS
data %>%
  ggplot() +
  aes(x = date, y = CPI) +
  geom_line(col = "#FC4E07", size = 1) +
  xlab("Date") + ylab("CPI") + ggtitle("CPI gross") +
  geom_smooth(method = "lm", size = 1)   
#Trend existence violates stationarity
#1st moment isn't constant, it violates stationarity

# -----------------------------------------------------------------------------

#Remove the trend
data <- data %>%
  mutate(
    log_CPI = log(CPI),
    d1log_CPI = difference(log(CPI), 1)*100, 
    d12log_CPI = difference(log(CPI), 12)*100
) %>%
  tail(-1)

#Plot the differenced CPI
data %>%
  ggplot() +
  geom_line(aes(date, log_CPI), col = "#FF0000", size = 0.2) +
  geom_line(aes(date, d1log_CPI), col = "#0000FF", size = 0.2) +
  geom_line(aes(date, d12log_CPI), col = "#00FF00", size = 0.2) +
  xlab("Date") + ylab("log_CPI") + 
  ggtitle("CPI logged") + labs(subtitle = "log(red) log1(blue) log12(green)")  
#Plot shows that the 12th order difference demonstrates higher variability 
#than the 1st order difference

#Remove the trend component (alternative approach)
decompDATA = decompose(log(DATA))
plot(decompDATA) #all data
plot(decompDATA$trend) #show the trend
plot(decompDATA$seasonal + decompDATA$random) #data without trend component

# -----------------------------------------------------------------------------

#Test for unit roots
#1st lib(urca)
data$log_CPI %>%
  ur.df(selectlags = "BIC", type ="trend") %>%
  summary()
#The trend is significant, tt=0.654
#Value of t-statistic is bigger than critical value 
#Fail to reject 0-h

#ADF test for unit roots, 2nd lib(tseries)
adf.test(log(DATA)) 
#p-value = 0.9696, fail to reject 0-h, unit root exists

#DF test for unit roots after the 1st order difference
data$d1log_CPI %>%
  ur.df(selectlags = "BIC", type ="drift") %>%
  summary()
#The trend is insignificant
#Reject 0-h, unit root doesn't exist, TS is stationary

#DF test for unit roots after the 12th order difference
data$d12log_CPI %>%
  tail(-11) %>%
  ur.df(selectlags = "BIC", type ="drift") %>%
  summary()
#The trend is insignificant
#Reject 0-h, unit root doesn't exist, TS is stationary

# -----------------------------------------------------------------------------

#Estimate AR(1)
ar1 <- data %>%
  model(ar1 = ARIMA(d1log_CPI ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)))

report(ar1) 
glance(ar1) 
#~0.54% of inflation for this period depends on its own previous values
#Growth rate of inflation for this period = 0.07%

# -----------------------------------------------------------------------------

#Find the appropriate lag. Use up to ten lags for AR
ar <- data %>%
  model(
    ar0 = ARIMA(d1log_CPI ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    ar1 = ARIMA(d1log_CPI ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    ar2 = ARIMA(d1log_CPI ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    ar3 = ARIMA(d1log_CPI ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    ar4 = ARIMA(d1log_CPI ~ 1 + pdq(4, 0, 0) + PDQ(0, 0, 0)),
    ar5 = ARIMA(d1log_CPI ~ 1 + pdq(5, 0, 0) + PDQ(0, 0, 0)),
    ar6 = ARIMA(d1log_CPI ~ 1 + pdq(6, 0, 0) + PDQ(0, 0, 0)),
    ar7 = ARIMA(d1log_CPI ~ 1 + pdq(7, 0, 0) + PDQ(0, 0, 0)),
    ar8 = ARIMA(d1log_CPI ~ 1 + pdq(8, 0, 0) + PDQ(0, 0, 0)),
    ar9 = ARIMA(d1log_CPI ~ 1 + pdq(9, 0, 0) + PDQ(0, 0, 0)),
    ar10 = ARIMA(d1log_CPI ~ 1 + pdq(10, 0, 0) + PDQ(0, 0, 0)),
  )
glance(ar)
glance(ar)[which.min(glance(ar)[["AIC"]]), ]
glance(ar)[which.min(glance(ar)[["BIC"]]), ]
#We should use the model with the 3rd lag, ar3
#For AR(3) AIC and BIC info criteria detect the same lag length 

# -----------------------------------------------------------------------------

#Visualize the order of PACF in a plot 
pacf <- data %>% 
  PACF(d1log_CPI, lag_max = 10) %>%
  autoplot() +
  xlab("Lags") +
  ylab("PACF")
pacf 
#2nd and 3rd lags are significant

acf <- data %>% 
  ACF(d1log_CPI, lag_max = 20) %>%
  autoplot() +
  xlab("Lags") +
  ylab("ACF")
acf
#From the shape of ACF we can derive that we need a "mixed AR&MA" model

# -----------------------------------------------------------------------------

#Test for AR(1)'s residual autocorrelation with up to 10 lags
LBtest <- ar1 %>%
  residuals() %>% 
  features(features = ljung_box, lag = 10) %>%
  print(LBtest)

# -----------------------------------------------------------------------------

#Estimate ARIMA(p,d,q) models to find the less complex one 
#From PACF plot we know that we need to test the models up to 3rd lag
arma <- data %>%
  model(
    arma00 = ARIMA(d1log_CPI ~ 1 + pdq(0, 0, 0) + PDQ(0, 0, 0)),
    arma10 = ARIMA(d1log_CPI ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arma20 = ARIMA(d1log_CPI ~ 1 + pdq(2, 0, 0) + PDQ(0, 0, 0)),
    arma30 = ARIMA(d1log_CPI ~ 1 + pdq(3, 0, 0) + PDQ(0, 0, 0)),
    arma01 = ARIMA(d1log_CPI ~ 1 + pdq(0, 0, 1) + PDQ(0, 0, 0)),
    arma11 = ARIMA(d1log_CPI ~ 1 + pdq(1, 0, 1) + PDQ(0, 0, 0)),
    arma21 = ARIMA(d1log_CPI ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)),
    arma31 = ARIMA(d1log_CPI ~ 1 + pdq(3, 0, 1) + PDQ(0, 0, 0)),
    arma02 = ARIMA(d1log_CPI ~ 1 + pdq(0, 0, 2) + PDQ(0, 0, 0)),
    arma12 = ARIMA(d1log_CPI ~ 1 + pdq(1, 0, 2) + PDQ(0, 0, 0)),
    arma22 = ARIMA(d1log_CPI ~ 1 + pdq(2, 0, 2) + PDQ(0, 0, 0)),
    arma32 = ARIMA(d1log_CPI ~ 1 + pdq(3, 0, 2) + PDQ(0, 0, 0)),
    arma03 = ARIMA(d1log_CPI ~ 1 + pdq(0, 0, 3) + PDQ(0, 0, 0)),
    arma13 = ARIMA(d1log_CPI ~ 1 + pdq(1, 0, 3) + PDQ(0, 0, 0)),
    arma23 = ARIMA(d1log_CPI ~ 1 + pdq(2, 0, 3) + PDQ(0, 0, 0)),
    arma33 = ARIMA(d1log_CPI ~ 1 + pdq(3, 0, 3) + PDQ(0, 0, 0))
  )

glance(arma)
glance(arma)[which.min(glance(arma)[["AIC"]]), ]
glance(arma)[which.min(glance(arma)[["BIC"]]), ]
#We should use the model with the 3rd lag in both terms, arma33
#For ARMA(33) AIC and BIC info criteria detect the same lag length

#Test for ARMA's residual autocorrelation
LBtest2 <- arma %>%
  residuals() %>% 
  group_by(.model) %>%
  features(features = ljung_box, lag = 10) %>%
print(LBtest2)
LBtest2[16,]

# -----------------------------------------------------------------------------

#Test for normality
data %>%
  select(d1log_CPI) %>%
  as.ts() %>%
  jarque.bera.test() 
#Find out the best model with ML:
glance(arma)[which.max(glance(arma)[["log_lik"]]), ]
 
