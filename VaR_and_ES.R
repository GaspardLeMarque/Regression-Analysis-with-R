#Required packages
library(quantmod)
library(moments)
library(MASS) # estimate the parameters of the scaled t-distribution
library(metRology) # create scaled t-distribution
library(rugarch) 

#Gold data
gold <- getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = F)
gold <- na.omit(gold)
gold <- gold["1979-12-31/2020-12-31"]
names(gold) <- "TR"

#Calculate returns
r <- diff(log(gold$TR))[-1]
R <- exp(r) - 1
round(tail(R,3),6) 

#Longer returns(weekly, monthly, quarterly, yearly)
r_w <- apply.weekly(r, sum)
r_m <- apply.monthly(r, sum)
r_q <- apply.quarterly(r, sum)
r_y <- apply.yearly(r, sum)

#Discrete returns from the log 
R_w <- exp(r_w) - 1
R_m <- exp(r_m) - 1
R_q <- exp(r_q) - 1
R_y <- exp(r_y) - 1

#VaR - Max loss in the portfolio, if we exclude the worst alpha% of possible outcomes (alpha quantile of the PDF)
#Assets*[exp(VaR)-1] 
mu <- mean(r)
sig <- sd(r)
var <- qnorm(0.05,mu,sig)
Assets <- 1000
HFvar <- Assets*(exp(var)-1)

#ES(cVaR) - expected return given that the return is worse than the associated VaR (Amount of money that we can lose in the worst scenario?)
#Average outcome for all the returns that fall into alpha% segment of the distro
es <- mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05
HFvar_es <- Assets*(exp(es)-1)

#Estimate VaR and ES by simulation
mu <- mean(r)
sig <- sd(r)
alpha <- 0.05
#Assuming normality of log returns
RNGkind(sample.kind="Rounding")
set.seed(123)
rvec_t <- rnorm(100000,mu,sig) #Theoretical
VaR_t <- quantile(rvec_t, alpha)
ES_t <- mean(rvec_t[rvec_t<VaR_t])
#Take the empirical distro
RNGkind(sample.kind="Rounding")
set.seed(123)
rvec_e <- sample(as.vector(r),100000,replace=TRUE) #Empirical
VaR_e <- quantile(rvec_e, alpha)
ES_e <- mean(rvec_e[rvec_e<VaR_e])

#Skewness, Kurtosis, Normality
rvec <- as.vector(r)
round(skewness(rvec),2)
round(kurtosis(rvec),2)
jarque.test(rvec)
#e.g. Wilshire index is more skewed to the left and has heavier tails than Gold

#Other tests for normality
#QQplot
#quantiles of observed data against the quantiles of assumed ND
#For normal data qqplot lays along the 45deg line
#KS test 
#compares the hist of observed data against the hist of the assumed ND

#Estimate params (Mu,Var,DoF) of the scaled t-distribution
t.fit <- fitdistr(rvec, 't')
round(t.fit$estimate,6)

#Estimate the VaR and ES at the 95% CI:
rvec <- rt.scaled(100000,
                  mean=t.fit$estimate[1],
                  sd=t.fit$estimate[2],
                  df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

#Estimate the VaR and ES on the 10-day horizon
#1st simulation - the scaled t-distribution
RNGkind(sample.kind="Rounding")
set.seed(123) 
rvec <- rep(0, 100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,
                         mean=t.fit$estimate[1],
                         sd=t.fit$estimate[2],
                         df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
round(VaR,6)
round(ES,6)

#2nd simulation - randomly sample observations from the empirical distribution
RNGkind(sample.kind="Rounding")
set.seed(123) 
rvec <- rep(0, 100000)
for (i in 1:10) {
  rvec <- rvec + sample(as.vector(r), 
                        100000, 
                        replace=TRUE)
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
round(VaR,6)
round(ES,6)

#3rd simulation - draw a block of consecutive observations
RNGkind(sample.kind="Rounding")
set.seed(123) 
rdat <- as.vector(r)
rvec <- rep(0, 100000)
posn <- seq(from=1, to=length(rdat)-9, by=1)
rpos <- sample(posn, 100000, replace=TRUE)
for (i in 1:10) {
  rvec <- rvec + rdat[rpos]
  rpos <- rpos + 1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR]) 
round(VaR,6)
round(ES,6)
#Autocorrelation exists if 2 and 3 methods give different results

#Serial correlation, volatility clustering
acf(r, 350)
acf(abs(r), 350)

#Estimate GARCH(1,1) model with standardized t-distribution
uspec <- ugarchspec(variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                    distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = r[,1]) 
fit.garch@fit$coef #estimated parameters

#Estimation output
savedmod <- cbind(r[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(savedmod) <- c("r", "s", "z") 

#ACF of the epsilon(z column)
acf(savedmod$z)
acf(abs(savedmod$z)) 

#Simulate 1-day outcomes by bootstrapping from the fitted epsilon
RNGkind(sample.kind="Rounding")
set.seed(123) 
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp")

#Save the outcomes
rvec <- boot.garch@fseries

#Calculate VaR and ES
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
#As a summary, after comparing ES of the TS that finish in 2017, 2008, 1987 
#for 1987 it wasn't that bad to come into the left tail than in 2008
#for 2017 it wasn't as severe as for 1987
#The more severe the crisis the higher the ES (price of a mistake?)
#But 1987 was the biggest drop of all time

#VaR and ES change over time with the volatility
