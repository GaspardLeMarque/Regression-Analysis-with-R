#Preamble
setwd("D:/R working dir")
y=function(){dev.new();x=par(no.readonly=T);dev.off();x}
par(y());options(scipen=0);dev.off();rm(list=ls())
Sys.setenv(LANG = "en")

library("dplyr")
library("tidyverse")
library("ggplot2")
library("tsibble")
library("lubridate")
library("fable") #TS Forecasting Models
library("readxl")
library("dynlm")
library("reshape2")
library("car")
library("vars")
library("systemfit")
library("lmtest") #Granger Causality test
library("magrittr") 
library("car") # Applied Regression
library("urca") #Unit Root test 

#Data
data <- read_excel("data_sheet2.xlsx")

#Create a tsibble obj 
data <- mutate(data, date = yearmonth(date)) %>%
  as_tsibble(index = date) 

# -----------------------------------------------------------------------------

#Plot
data %>%
  ggplot() +
  geom_line(aes(date, CPI), col = "#FF0000", size = 1) +
  geom_line(aes(date, FEDFUNDS), col = "#00FF00", size = 1) +
  geom_line(aes(date, UNRATE), col = "#0000FF", size = 1) +
  xlab("Date") + ylab("Change in percentages") + 
  ggtitle("CPI + FEDFUNDS + UNRATE") + 
  labs(subtitle = "CPI(red) FEDFUNDS(green) UNRATE(blue) ")
#From glance CPI isn't stationary, but FEDFUNDS and UNRATE are stationary

#As an option, can plot all 3 TS separately
data %>%
  autoplot(vars(CPI)) +
  geom_line(col = "#e30e0e", size = 0.5) +
  xlab("Year") + ylab("CPI")

data %>%
  autoplot(vars(FEDFUNDS)) +
  geom_line(col = "#e30e0e", size = 0.5) +
  xlab("Year") + ylab("FEDFUNDS")

data %>%
  autoplot(vars(UNRATE)) +
  geom_line(col = "#e30e0e", size = 0.5) +
  xlab("Year") + ylab("UNRATE")

# -----------------------------------------------------------------------------

#Check for stationarity using regression
#CPI
fit_trend <- lm(CPI ~ date, data = data)
summary(fit_trend)
#There's a trend in CPI, coefficient is significant at the 5% level.
#Series is not stationary
fit_trend <- lm(FEDFUNDS ~ date, data = data)
summary(fit_trend)
#There's a trend in FEDFUNDS, coefficient is significant at the 5% level. 
#Series is not stationary
fit_trend <- lm(UNRATE ~ date, data = data)
summary(fit_trend)
#There's a trend in UNRATE, coefficient is insignificant at the 5% level. 
#Series is stationary
#In general, stationarity check should be done before making tests and inference

#Check for stationarity using Unit Root test
#H0: stochastic trend
summary(ur.df(data$CPI, selectlags = "BIC", type ="trend"))
#t-statistic = -3.915, CPI is trend-stationary
summary(ur.df(data$FEDFUNDS, selectlags = "BIC", type ="trend"))
#t-statistic = -3.3275, FEDFUNDS is trend-stationary
summary(ur.df(data$UNRATE, selectlags = "BIC", type ="trend"))
#t-statistic = -1.2859, UNRATE is trend-stationary

# -----------------------------------------------------------------------------

#Transform data to Time Series objects
data <- data %>%
  mutate(CPI = difference(log(CPI), 12) * 100) %>%
  tail(-12)

data <- data %>%
  mutate(
    CPI = ts(CPI, start = 1961, frequency = 12),
    FEDFUNDS = ts(FEDFUNDS, start = 1961, frequency = 12),
    UNRATE = ts(UNRATE, start = 1961, frequency = 12)
  ) 
#Check the class of data
class(data)

#Plot transformed data
data %>%
  ggplot() +
  geom_line(aes(date, CPI), col = "#FF0000", size = 1) +
  geom_line(aes(date, FEDFUNDS), col = "#00FF00", size = 1) +
  geom_line(aes(date, UNRATE), col = "#0000FF", size = 1) +
  xlab("Date") + ylab("Change in percentages") + 
  ggtitle("CPI + FEDFUNDS + UNRATE") + 
  labs(subtitle = " red + green + blue ")

# -----------------------------------------------------------------------------

#Select the best fitted VAR model and estimate it
VAR <- as.data.frame(data[, c("CPI", "FEDFUNDS", "UNRATE")])
#Find the lag structure
#The VAR system should be stable and without autocorrelation in the residuals
VARselect(VAR, lag.max = 4, type = c("const"))
fit_s <- vars::VAR(VAR, p = 3, type = "const") 

#Doing lapply() instead of the loop
residuals2 <- lapply(fit_s$var, residuals)

#Ljung-Box test with a lag order = 12
#H0: no Autocorrelation
box_tests1 <- lapply(residuals2, function(x) Box.test(x, lag = 12, type = "Ljung-Box"))

box_tests1$CPI 
box_tests1$UNRATE 
box_tests1$FEDFUNDS 
#Autocorrelation still exists

#Ljung-Box test with a lag order = 3
box_tests2 <- lapply(residuals2, function(x) Box.test(x, lag = 3, type = "Ljung-Box"))

box_tests2$CPI
box_tests2$UNRATE 
box_tests2$FEDFUNDS
#With 3 lags there is no Autocorrelation in the UNRATE, 
#CPI and FEDFUNDS still have it

# -----------------------------------------------------------------------------

#Re-estimate the VAR model after increasing the order by 1
fit3 <- vars::VAR(VAR, p = 4, type = "const")
residuals3 <- lapply(fit3$var, residuals)

#Ljung-Box test with a lag order = 12
box_tests3 <- lapply(residuals3, function(x) Box.test(x, lag = 12, type = "Ljung-Box"))

box_tests3$CPI 
box_tests3$UNRATE 
box_tests3$FEDFUNDS
#Still have Autocorrelation in all 3 cases

#Ljung-Box test with a lag order = 3
box_tests4 <- lapply(residuals3, function(x) Box.test(x, lag = 3, type = "Ljung-Box"))

box_tests4$CPI
box_tests4$UNRATE
box_tests4$FEDFUNDS
#No Autocorrelation in all 3 cases
