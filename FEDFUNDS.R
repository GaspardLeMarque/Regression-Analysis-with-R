#Data
data <- fread("MPdata.csv")
names(data) <- c("Date", "FEDFUNDS", "INFLATION", "GDP")

#Parsing
data <- mutate(data,
               Date = yearquarter(ymd(Date)),
               GDP = sapply(data$GDP, gsub, pattern = ",", replacement= "."),
               FEDFUNDS = sapply(data$FEDFUNDS, gsub, pattern = ",", replacement= "."),
               INFLATION = sapply(data$INFLATION, gsub, pattern = ",", replacement= ".")) 

data <- mutate(data,
               GDP = sapply(data$GDP, as.numeric),
               FEDFUNDS = sapply(data$FEDFUNDS, as.numeric),
               INFLATION = sapply(data$INFLATION, as.numeric)) %>%
        as_tsibble(index = Date)

#Transform the data
data <- data %>% 
  mutate(OutputGap = difference(log(GDP), 1) * 100) %>%
  tail(-1) 

#Check the trend existence
summary(fit_trend <- lm(OutputGap ~ Date, data = data))
#Coefficient is significant at 1% level => trend exists

#Add a residuals column to the data
data <- cbind(data, Residuals = residuals(fit_trend))

#Plot the Output Gap Residuals
data %>%
  ggplot(aes(Date, Residuals)) +
  geom_line(col = "#0c2ac1") +
  xlab("Years") + ylab("Units") +
  ggtitle("Output Gap Residuals") +
  theme(plot.title = element_text(hjust = 0.5))

#Plot all data separately 
data %>%
  autoplot(vars(FEDFUNDS, INFLATION, OutputGap)) +
  geom_line(col = "#0c2ac1") +
  xlab("Years")

#---------------------------------------------------------------------------------

#Run linear regression to check the Taylor rule assumption
fit <- dynlm(FEDFUNDS ~ INFLATION + OutputGap, data = data)
summary(fit)
#the linear trend coefficients are significant at the 0.1% significance level
#When inflation increases by 1% the rate increases by 0.99%
#When Output Gap increases by 1% the rate increases by 0.55%
#The results are consistent with the Taylor rule

#Predicted rate based on linear model
predFEDFUNDS <- predict.lm(fit)
#Adding NA's to make the vector compatible with the main data frame
c(rep(NA, length(data$FEDFUNDS) - length(predFEDFUNDS)), predFEDFUNDS)
length(predFEDFUNDS) <- 271

#Taylor rule rate
trFEDFUNDS <- 1 + 1.5*data$INFLATION + 0.5*data$OutputGap

#Add 2 new TS to the data frame
data <- cbind(data, Predicted = predFEDFUNDS, Taylor = trFEDFUNDS)

#Make an interest rates plot to compare 3 types of rates 
data %>%
  ggplot() +
  geom_line(aes(Date, FEDFUNDS, color = "Federal funds rate"), size = 0.5) +
  geom_line(aes(Date, Predicted, color = "Predicted rate"), size = 0.5) +
  geom_line(aes(Date, Taylor, color = "Taylor rate"), size = 0.5) +
  xlab("Years") + ylab("%") +
  ggtitle("Interest Rates") +
  scale_colour_manual("", 
                      values = c("#00FF00", "#FF0000", "#0000FF")) +
  theme(legend.position = c(.95, .98),
        legend.justification = c("right", "top"),
        plot.title = element_text(hjust = 0.5))
#According to the Taylor Rule:
#In 2004-2005 period the interest rate should have been higher.
#In 2009-2010 period the rate should have been lower, but not significantly.
#In 2012-2015 period the rate should have been significantly higher.
