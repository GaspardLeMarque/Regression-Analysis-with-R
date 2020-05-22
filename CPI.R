#Preamble
setwd("D:/R working dir")
y=function(){dev.new();x=par(no.readonly=T);dev.off();x}
par(y());options(scipen=0);dev.off();rm(list=ls())

#Packages
install.packages("tidyverse")
install.packages("rstudioapi")
install.packages("tsibble")
install.packages("tseries")
install.packages("fable")
install.packages("feasts")
library("rstudioapi")
library("tidyverse")
library("tsibble")
library("lubridate")
library("tseries") #for unit root test
library("fable")
library("feasts")


#Data
data <- read_csv("data_sheet1.csv")

#Create a TS obj (as an ordered data)
data <- mutate(data, date = dmy(date)) %>%
  tsibble(index = date)

#Plot
data %>%
  ggplot() +
  aes(x = date, y = CPI) +
  geom_line(col = "#FC4E07", size = 1) +
  xlab("Date") + ylab("CPI") + ggtitle("CPI gross") +
  geom_smooth(method = "lm", size = 1)  #trend line shows that we have a trend
  
  
DATA <- ts(data[,2], start=1960, frequency = 12) # create a TS obj (vector)

#Remove the trend
#We have a stochastic trend, to remove it we need to do differencing
data_log <- data %>%
  mutate(
    log_CPI = log(CPI),
    d1log_CPI = difference(log(CPI), 1)*100, 
    d12log_CPI = difference(log(CPI), 12)*100
)

#Plot the differenced CPI
data_log %>%
  ggplot() +
  geom_line(aes(date, log_CPI), col = "red", size = 0.2) +
  geom_line(aes(date, d1log_CPI), col = "blue", size = 0.2) +
  geom_line(aes(date, d12log_CPI), col = "green", size = 0.2) +
  xlab("Date") + ylab("log_CPI") + 
  ggtitle("CPI logged") + labs(subtitle = "log(red) log1(blue) log12(green)")  

#Remove the trend component (alternative approach)
decompDATA = decompose(log(DATA))
plot(decompDATA) #all data
plot(decompDATA$trend) #show the trend
plot(decompDATA$seasonal + decompDATA$random) #data without trend component

#Test for unit roots
adf.test(DATA) #p-value = 0.7846, fail to reject 0-h, unit root exists



#Estimate AR(1)
ar1 <- data_log %>%
  model(ar1 = ARIMA(log_CPI ~ 1 + pdq(1, 0, 0) + PDQ(0, 0, 0)))

report(ar1)

#Use PACF to decide which lag to use
pacf <- data_log %>% 
  PACF(d1log_CPI, lag_max = 20) %>%
  autoplot() +
  xlab("Lags") +
  ylab("PACF")
pacf

#Test for residual autocorrelation
LBtest <- ar1 %>%
  residuals() %>% 
  features(features = ljung_box, lag = 10)



