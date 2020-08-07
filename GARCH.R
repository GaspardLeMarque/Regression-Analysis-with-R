#Preamble
setwd("D:/R working dir")
y=function(){dev.new();x=par(no.readonly=T);dev.off();x}
par(y());options(scipen=0);dev.off();rm(list=ls())
Sys.setenv(LANG = "en")

library("readxl")
library("magrittr") 
library("tidyverse")
library("ggplot2")
library("tsibble")
library("fable")
library("fabletools")
library("feasts") 
library("lubridate")
library("dplyr")
library("moments") 
library("quantmod")
library("rugarch")
library("urca")
library("feasts") 

#Data
data <- read_excel("EuroStoxx50E.xlsx")
data <- mutate(data, Date = ymd(Date)) %>%
  as_tsibble(index = Date)

#Plot
data %>%
  autoplot(vars(Close)) +
  geom_line(col = "#db0e0e") +
  xlab("Trading Day") + ylab("Index Units") +
  ggtitle("Close Price") +
  theme(plot.title = element_text(hjust = 0.5))

#----------------------------------------------------------

#Transformation check
#Check for a trend existence using regression
summary(fit_trend <- lm(Close ~ Date, data = data)) 
#Coefficient is significant at 0.1% and p-value = < 2.2e-16
#We have a linear dependence = trend exists 

#Unit root test (stochastic or trend stationary)
summary(ur.df(data$Close, selectlags = "BIC", type ="trend"))
#tau3 is accepted at 5pct => there is a unit root
#phi2 is accepted since 3.4936 < 4.03 critical value
#phi3 is accepted since 5.2257 < 5.34 critical value
#t-statistic = -3.2167 => TS is trend-stationary

#Differencing and creation of the trading day vector
data <- data %>%  
  mutate(Return = difference(log(Close), 1) * 100) %>%
  tail(-1) %>%
  arrange(Date) %>% 
  mutate(TradingDay = row_number(Date)) %>%
  as_tsibble(index = TradingDay)

#Plot Returns
data %>%
  ggplot() +
  geom_line(aes(Date, Return), col = "#db0e0e", size = 0.5) +
  xlab("Date") + ylab("Return") + 
  ggtitle("EUROSTOXX50 Return") +
  theme(plot.title = element_text(hjust = 0.5))

#Plot the histogram of the Continuous Returns
#and compare it with Normal Distribution
data.frame(x = rnorm(length(data$Return)), return = data$Return) %>%
  ggplot() +
  geom_histogram(aes(return, y = ..density..), bins = 60,
                 alpha = 0.5, fill = "#0000FF", col = "#000000") +
  stat_function(fun = dnorm, aes(x = x, color = "Normal Distribution"), size = 1) +
  xlim(c(-15, 15)) + 
  ylab("Density") + xlab("") +
  ggtitle("EUROSTOXX50 Return") +
  scale_colour_manual("Distribution:", values = c("#FF0000")) +
  theme(legend.position = c(.95, .98),
        legend.justification = c("right", "top"),
        plot.title = element_text(hjust = 0.5))
#Distribution is not normal, it has fat tails and more mass at the center 

#Kurtosis calculation
K = data$Return[!is.na(data$Return)] %>% 
  kurtosis() 
#K = 8.56, K > 3 => excess kurtosis

#----------------------------------------------------------

#Plot the t-distribution with the custom DoF based on excess
df1 = 6/(K-3) + 4

data.frame(x = rnorm(length(data$Return)), return = data$Return) %>%
  ggplot() +
  geom_histogram(aes(return, y = ..density..), bins = 60,
                 alpha = 0.5, fill = "#0000FF", col = "#000000") +
  stat_function(fun = dt, args = list(df = df1), aes(x = x, color = "t-Distribution"), size = 1) +
  stat_function(fun = dnorm, aes(x = x, color = "Normal Distribution"), size = 1) +
  xlim(c(-15, 15)) + 
  ylab("Density") + xlab("") +
  ggtitle("EUROSTOXX50 Return Distribution") +
  scale_colour_manual("Distribution:", values = c("#00FF00", "#FF0000")) +
  theme(legend.position = c(.95, .98),
        legend.justification = c("right", "top"),
        plot.title = element_text(hjust = 0.5))

#----------------------------------------------------------

#Estimate an ARIMA(2,0,1) model
fit <- data %>% 
  model(arma21 = ARIMA(Return ~ 1 + pdq(2, 0, 1) + PDQ(0, 0, 0)))

fit %>%
  select(arma21) %>%
  fabletools::report()
#only AR1 term is significant.
#On the 1st lag of AR term 78% of the previous period variation 
#goes to the next period.
#Average growth of returns is 0.13%

fit %>%
  augment() %>%
  features(.resid, ljung_box, lag = 10) 
#Failed to reject H0 => no autocorrelation in the residuals

fit %>%
  augment() %>% 
  features(.resid^2, ljung_box, lag = 10) 
#Reject H0 => autocorrelation in the squared residuals
#ARIMA(2,0,1) is not the best fit to describe the given process
#The reason is autocorrelation between squared residuals
#Therefore, unconditional homoskedasticity is detected
#Also we have an excess kurtosis that points out the existence of ARCH effects
#So we need to use conditional variance models

#----------------------------------------------------------

#5.Find another linear model, try ARCH-ARIMA model
acf(data$Return, lag.max=30) 
pacf(data$Return, lag.max=30)
#ACF,PACF suggest to use ARIMA(5,0,1) and ARCH(5,0)
#PACF dies out after the 5th lag

specARCH5 <- ugarchspec(                            
  mean.model         = list(armaOrder = c(5, 1)),
  variance.model     = list(model = "sGARCH", garchOrder = c(5, 0)),
  distribution.model = "norm")

ARCH5 <- ugarchfit(specARCH5, na.omit(data$Return))
ARCH5
#AR part shows that an increase in returns by 1% 5 days ago,
#leads to a decrease by 6%.
#Constant term in the AR is positive and significant (0.006226)
#Variance part shows a positive constant term,
#All variance terms are positive and significant,
#After the shock 5 days ago conditional variance is 14% larger.

#Find the sum of alphas from the Optimal Parameters to check the persistence
#of the conditional variance term
sum(ARCH5@fit$matcoef[9:13,1])
#0.77 < 1 => conditional variance is persistent
#There's no non-stationary conditional variance

#Find standardized residuals for a heteroskedasticity test
#Residuals
residARCH5 <- resid(ARCH5@fit)

#Conditional Variance
condvarARCH5 <- c(ARCH5@fit$var)

#Standardize Residuals by Conditional Standard Deviation
residstdARCH5 <- residARCH5/I(sqrt(condvarARCH5))

#Check the form of the distribution
kurtosis(residstdARCH5)
#Kurtosis = 5.328081 > 3 => excess kurtosis
skewness(residstdARCH5)
#Skewness = -0.2928536
#tail is larger on the left side of the distribution

#Ljung-Box test for autocorrelation in standardized residuals
Box.test(residstdARCH5, lag = 10, type = "Ljung-Box") 
#p-value = 0.7949, Failed to reject the H0
#no autocorrelation in the 1st moment after the 10th lag

#Ljung-Box test for autocorrelation in squared standardized residuals
Box.test(residstdARCH5^2, lag = 10, type = "Ljung-Box") 
#p-value = 0.04223, Reject the H0
#With 10 lags it is an autocorrelation in the squared residuals,
#Need to use a better model

#With the other combinations of lags it is possible to have
#a bigger p-value for the squared residuals LB test, closer to 1
#For example, ARMA(5,5)-GARCH(2,2)  
#However it doesn't make sense, because in the next step 
#it's gonna be a EGARCH(1,1) model, which is better than sGARCH
#According to D.B. Nelson. Conditional heteroskedasticity in asset returns: 
#A new approach. Econometrica, 59(2):347{70, 1991.

#----------------------------------------------------------

#Estimate a EGARCH(1,1) model
specEGARCH11 <- ugarchspec(
  mean.model = list(armaOrder = c(5, 1)),
  variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

EGARCH11 <- ugarchfit(specEGARCH11, na.omit(data$Return))
EGARCH11
#AR part shows that an increase in returns by 1% 5 days ago,
#leads to a decrease by 3.5%.
#Constant term in the AR is positive and insignificant
#Beta shows that 98% of previous period conditional variance comes to this period
#Alpha shows that -8.9% of shock from previous period comes to this period
#AR1, MA1 and all terms in the EGARCH part are significant
#Constant term in a variance part is positive
#This TS is weakly efficient

#Sum up the alphas to check the persistence of the conditional variance term
sum(EGARCH11@fit$matcoef[9:11,1]) 
#1.02 > 1 => conditional variance is more persistent than in ARCH model
#But the fact that the value > 1, 
#suggests that there's a poor conditional mean specification 

#Find standardized residuals for a heteroskedasticity test
#Repeat the procedure of finding the standardized residuals
residEGARCH11 <- resid(EGARCH11@fit)
condvarEGARCH11 <- c(EGARCH11@fit$var)
residstdEGARCH11 <- residEGARCH11/I(sqrt(condvarEGARCH11))

#Check the form of the distribution
kurtosis(residstdEGARCH11)
#Kurtosis = 6.330655 > 3 => excess kurtosis
skewness(residstdEGARCH11)
#Skewness = -0.3024513
#tail is larger on the left side of the distribution
#Kurtosis is higher in this model, but the skewness is almost the same

#Ljung-Box test 
Box.test(residstdEGARCH11, lag = 10, type = "Ljung-Box")
#p-value = 0.9529, Failed to reject the H0
#no autocorrelation in the 1st moment after the 10th lag
#p-value increased in comparison with the previous test

#Test for conditional heteroskedasticity
Box.test(residstdEGARCH11^2, lag = 10, type = "Ljung-Box") 
#p-value = 0.965, Failed to reject the H0
#Obtained a much bigger p-value than in the ARCH test
#GARCH model shows better results
#no autocorrelation in the 2nd moment
#conditional heteroskedasticity doesn't matter = no ARCH effects
#There is no necessity to include higher order (G)ARCH terms 

#----------------------------------------------------------

#Estimate a GJR-GARCH(1,1) model
specGJRGARCH11 <- ugarchspec(
  mean.model = list(armaOrder = c(5, 1)),
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  distribution.model = "norm"
)

GJRGARCH11 <- ugarchfit(specGJRGARCH11, na.omit(data$Return))
GJRGARCH11
#AR part shows that an increase in returns by 1% 5 days ago,
#leads to a decrease by 3.9%.
#Constant term in the AR is positive and insignificant
#Beta shows that 89% of previous period conditional variance comes to this period
#Alpha shows that 2.7% of shock from previous period comes to this period
#Only terms in the GARCH part are significant
#Constant term in a variance part is positive and significant

#Sum up the alphas to check the persistence of the conditional variance term
sum(GJRGARCH11@fit$matcoef[9:11,1]) 
#1.04 > 1 => conditional variance is more persistent than in EGARCH model
#But again there's a poor conditional mean specification

#Find standardized residuals for a heteroskedasticity test
#Repeat the procedure of finding the standardized residuals
residGJRGARCH11 <- resid(GJRGARCH11@fit)
condvarGJRGARCH11 <- c(GJRGARCH11@fit$var)
residstdGJRGARCH11 <- residGJRGARCH11/I(sqrt(condvarGJRGARCH11))

#Check the form of the distribution
kurtosis(residstdGJRGARCH11)
#Kurtosis = 5.987394 > 3 => excess kurtosis
skewness(residstdGJRGARCH11)
#Skewness = -0.2996382
#tail is larger on the left side of the distribution
#Kurtosis is lower in this model, the skewness is again closer to the ARCH model

#Ljung-Box test 
Box.test(residstdGJRGARCH11, lag = 10, type = "Ljung-Box")
#p-value = 0.9826, Failed to reject the H0
#no autocorrelation in the 1st moment after the 10th lag
#p-value increased even more in comparison with the previous test

#Test for conditional heteroskedasticity
Box.test(residstdGJRGARCH11^2, lag = 10, type = "Ljung-Box") 
#p-value = 0.9868, Failed to reject the H0
#Also a much bigger p-value than in the EGARCH test
#Now p-value is almost 1 which is a more confident result

#LB test shows that GJR-GARCH is the best model for this stochastic process
#However the overall significance of variance terms in EGARCH is better
#EGARCH shows more persistence (0.98 > 0.90), 
#but the betas < 1 in both cases => stationary conditional variance

#Leverage effect occurs only in the EGARCH
GJRGARCH11@fit$matcoef[9,1] #alpha = 0.02767551
EGARCH11@fit$matcoef[9,1] #alpha = -0.08959311
#So in case of the negative shock EGARCH shows the bigger value

#Negative shock
-1*(EGARCH11@fit$matcoef[9,1]) + EGARCH11@fit$matcoef[11,1] #0.2150036
-1*(GJRGARCH11@fit$matcoef[9,1]) + GJRGARCH11@fit$matcoef[11,1] #0.08730312

#Positive shock
EGARCH11@fit$matcoef[9,1] + EGARCH11@fit$matcoef[11,1] #0.03581738
GJRGARCH11@fit$matcoef[9,1] + GJRGARCH11@fit$matcoef[11,1] #0.1426541
#It seems that in GJRGARCH we have a higher volatility after a positive shock
