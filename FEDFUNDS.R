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
