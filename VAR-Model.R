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

#Plot all 3 TS separately because CPI index is in units and 
#FEDFUNDS and UNRATE are in percentage points
data %>%
  autoplot(vars(CPI)) +
  geom_line(col = "#e30e0e", size = 0.5) +
  xlab("Year") + ylab("Index Units") +
  ggtitle("CPI") +
  theme(plot.title = element_text(hjust = 0.5))

data %>%
  autoplot(vars(FEDFUNDS)) +
  geom_line(col = "#e30e0e", size = 0.5) +
  xlab("Year") + ylab("% points") +
  ggtitle("Federal Funds") +
  theme(plot.title = element_text(hjust = 0.5))

data %>%
  autoplot(vars(UNRATE)) +
  geom_line(col = "#e30e0e", size = 0.5) +
  xlab("Year") + ylab("% points") +
  ggtitle("Unemployment Rate") +
  theme(plot.title = element_text(hjust = 0.5))
#From glance CPI isn't stationary, but FEDFUNDS and UNRATE are stationary

# -----------------------------------------------------------------------------

#Check for a trend existence using regression
#CPI
fit_trend <- lm(CPI ~ date, data = data) 
summary(fit_trend)
#There's a trend in CPI, coefficient is significant at the 5% level.
#p-value = < 2.2e-16 
fit_trend <- lm(FEDFUNDS ~ date, data = data)
summary(fit_trend)
#There's a trend in FEDFUNDS, coefficient is significant at the 5% level.
#p-value = < 2.2e-16
fit_trend <- lm(UNRATE ~ date, data = data)
summary(fit_trend)
#There's not a visible trend in UNRATE, coefficient is insignificant at the 5% level.
#p-value = 0.1944

#Check for stationarity using Unit Root test
#H0: stochastic trend
summary(ur.df(data$CPI, selectlags = "BIC", type ="trend"))
#t-statistic = -3.915, CPI is trend-stationary
#value is inside the rejection region (1pct-5pct)
summary(ur.df(data$FEDFUNDS, selectlags = "BIC", type ="trend"))
#t-statistic = -3.3275, FEDFUNDS is trend-stationary
#value is inside the rejection region (5pct-10pct)
summary(ur.df(data$UNRATE, selectlags = "BIC", type ="trend"))
#t-statistic = -1.2859, UNRATE is stationary
#value is OUTSIDE the rejection region (10pct=-3.12)

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

#Plot transformed data separately
data %>%
  autoplot(vars(CPI, UNRATE, FEDFUNDS))

# -----------------------------------------------------------------------------

#Combine 3 plots using multiplot() function
#Define Multiplot() 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  #Count the number of objects for input
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  #Use 'cols' to determine layout
  if (is.null(layout)) {
    #Make the panel
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  #Start plotting
  if (numPlots==1) {
    print(plots[[1]])
    
    #Make the grid
  } else {
    grid.newpage() 
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    #Place the plots by matching positions with the i,j matrix
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Make combined plot
p1 <- ggplot(data, aes(date, CPI)) +
  geom_line(col="#FF0000") +
  xlab("Year") + ylab("Index Units") +
  ggtitle("CPI") +
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(data, aes(date, FEDFUNDS)) +
  geom_line(col="#00FF00") +
  xlab("Year") + ylab("% points") +
  ggtitle("Federal Funds") +
  theme(plot.title = element_text(hjust = 0.5))

p3 <- ggplot(data, aes(date, UNRATE)) +
  geom_line(col="#0000FF") +
  xlab("Year") + ylab("% points") +
  ggtitle("Unemployment") +
  theme(plot.title = element_text(hjust = 0.5))

multiplot(p1, p2, p3, cols=3)

# -----------------------------------------------------------------------------

#Select the best fitted VAR model and estimate it
VAR <- as.data.frame(data[, c("CPI", "FEDFUNDS", "UNRATE")])
#Find the lag structure
#The VAR system should be stable and without autocorrelation in the residuals
VARselect(VAR, lag.max = 4, type = c("const"))
fit_s <- vars::VAR(VAR, p = 3, type = "const") 

#Doing lapply() instead of the loop
residuals2 <- lapply(fit_s$var, residuals)

#Ljung-Box test with a lag = 12
#H0: no Autocorrelation
box_tests1 <- lapply(residuals2, function(x) Box.test(x, lag = 12, type = "Ljung-Box"))

box_tests1$CPI 
box_tests1$UNRATE 
box_tests1$FEDFUNDS 
#Autocorrelation still exists

# Ljung-Box test with a lag = 3
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

#Ljung-Box test with a lag = 12
box_tests3 <- lapply(residuals3, function(x) Box.test(x, lag = 12, type = "Ljung-Box"))

box_tests3$CPI 
box_tests3$UNRATE 
box_tests3$FEDFUNDS
#Still have Autocorrelation in all 3 cases

#Ljung-Box test with a lag = 3
box_tests4 <- lapply(residuals3, function(x) Box.test(x, lag = 3, type = "Ljung-Box"))

box_tests4$CPI
box_tests4$UNRATE
box_tests4$FEDFUNDS
#No Autocorrelation in all 3 cases
