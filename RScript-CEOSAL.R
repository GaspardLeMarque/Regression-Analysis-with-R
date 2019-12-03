#CEOSALARY
library(foreign)
install.packages('e1071')
library(e1071)
install.packages('haven')
library(haven)
install.packages('ggplot2')
library(ggplot2)
load('part2_CEOSAL.Rdata')
attach(CEOSAL)
#First inspection
n = length(salary)
summary(CEOSAL)
#Medians
median(ceoten)
median(salary)
#Compare with means
mean(ceoten)
mean(salary)
skewness(ceoten)
skewness(salary)
#salary skewed more than ceoten
#median(ceoten) < mean(ceoten) => destribution of ceoten is rigth-skewed
#median(salary) < mean(salary) => destribution of salary is rigth-skewed
#Create a logical vector which is TRUE at every location where 0 occurs
sum(ceoten == 0)
#5 CEO's are in their 1st year
max(ceoten)
#37 years is the logest tenure for the CEO
cov(salary, grad)
#cov has '-' sign
#categorical var "grad" and a response var "salary" are negatively related
cor(profits, mktval)
#vars "profits" and "mktval" are positively linear related
#this value is close to 1 (0.918128)
cor.test(profits, mktval, method = c("pearson"))
#p-value < 5% (p-value < 2.2e-16) so the correlation is significant
#New Variables
salaryLog = log(salary)
salesLog = log(sales)
ageQuad = poly(age, degree = 2)
ceotenQuad = poly(ceoten, degree = 2)
profOverSls = (profits/sales)*100
#Graphical illustration
#Plot salary w density plot
ggplot(CEOSAL, aes(salary)) + geom_density(fill = "#0000ff")
#Scatter plots
scatter.smooth(x=sales, y=salary, main="salary ~ sales")
scatter.smooth(x=salesLog, y=salaryLog, main="salary ~ sales")
#Linear regression
#2.6 
#Estimate simple regression model
lm(salary~ceoten, data = CEOSAL)
slopeCeoten = cov(salary,ceoten)/var(ceoten)
interceptCeoten = mean(salary) - (slope * mean(ceoten))
#Intercept at 772.43 and slope = 11.75
summary(lm(salary~sales, data = CEOSAL)) 
#Intercept at 7.364 and slope = 0.036
summary(lm(salary~salesLog, data = CEOSAL)) #1% incr of sales 1772 salary incr
#Intercept at -415.1 and slope = 177.1
summary(lm(salaryLog~sales, data = CEOSAL))
#Intercept at 6.439 and slope = 4.079e-05
summary(lm(salaryLog~salesLog, data = CEOSAL))
#Intercept at 4.9611, and slope = 0.2243 
#log-log Model
#by 1% change in sales, salary changes by 0.22%
#the impact of the sales is positive but becomes smaller as the value increases
#Covariance matrix
model_covmatr <- lm(salaryLog~salesLog)
slope = cov(salaryLog,salesLog)/var(salesLog)
intercept = mean(salaryLog) - slope * mean(salesLog)
summary(lm(salaryLog ~ salesLog))
sqrt(diag(vcov(lm(salaryLog ~ salesLog))))
