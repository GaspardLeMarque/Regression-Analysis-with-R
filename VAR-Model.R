#Preamble
setwd("D:/R working dir")
y=function(){dev.new();x=par(no.readonly=T);dev.off();x}
par(y());options(scipen=0);dev.off();rm(list=ls())

library("rstudioapi")
library("dplyr")
library("tidyverse")
library("tsibble")
library("lubridate")
library("fable")
library("readxl")
library("dynlm")
library("reshape2")
library("car")
library("vars")
library("systemfit")
library("lmtest")

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

# -----------------------------------------------------------------------------

#Transform data
data <- data %>%
  mutate(CPI = difference(log(CPI), 12) * 100) %>%
  tail(-12)

data <- data %>%
  mutate(
    CPI = ts(CPI, frequency = 12),
    FEDFUNDS = ts(FEDFUNDS, frequency = 12),
    UNRATE = ts(UNRATE, frequency = 12)
  ) 

#Plot with ts
data %>%
  ggplot() +
  geom_line(aes(date, CPI), col = "#FF0000", size = 1) +
  geom_line(aes(date, FEDFUNDS), col = "#00FF00", size = 1) +
  geom_line(aes(date, UNRATE), col = "#0000FF", size = 1) +
  xlab("Date") + ylab("Change in percentages") + 
  ggtitle("CPI + FEDFUNDS + UNRATE") + 
  labs(subtitle = " red + green + blue ")

# -----------------------------------------------------------------------------

#Select and estimate the "reduced" form VAR model
VAR <- as.data.frame(data[, c("CPI", "FEDFUNDS", "UNRATE")])
VARselect(VAR, lag.max = 12, type = c("const"))

fit_s <- vars::VAR(VAR, p = 1, type = "const") 
fit_s
