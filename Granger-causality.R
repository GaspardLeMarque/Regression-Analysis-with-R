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
library("fable")
library("readxl")
library("dynlm")
library("car")
library("vars")
library("systemfit")
library("lmtest") # Granger Causality test
library("magrittr") 
library("car") # Applied Regression

#Data
data <- read_excel("data_sheet2.xlsx")

#Create a tsibble obj 
data <- mutate(data, date = yearmonth(date)) %>%
  as_tsibble(index = date) 

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

# -----------------------------------------------------------------------------

#Granger test for multivariate independence
#H0 = no GC
# Test CPI and UNRATE, control FEDFUNDS
multi_CPI <- dynlm(CPI ~ L(CPI,1:6) + L(UNRATE,1:6) + L(FEDFUNDS,1:6), data = data)
multi_UNRATE <- dynlm(UNRATE ~ L(CPI,1:6) + L(UNRATE,1:6) + L(FEDFUNDS,1:6), data = data)
multi_inst_CPI <- dynlm(CPI ~ L(CPI,1:6) + L(UNRATE,0:6) + L(FEDFUNDS,0:6), data = data)

linearHypothesis(multi_CPI, paste0("L(UNRATE, 1:6)", 1:6, "=0")) # UNRATE -> CPI = no
linearHypothesis(multi_UNRATE, paste0("L(CPI, 1:6)", 1:6, "=0")) # CPI -> UNRATE = no
linearHypothesis(multi_inst_CPI, "L(UNRATE, 0:6)0=0") # CPI - UNRATE = no

# CPI and FEDFUNDS, control UNRATE
multi_FEDFUNDS <- dynlm(FEDFUNDS ~ L(CPI,1:6) + L(FEDFUNDS,1:6) + L(UNRATE,1:6), data = data)
multi_CPI <- dynlm(CPI ~ L(CPI,1:6) + L(FEDFUNDS,1:6) + L(UNRATE,1:6), data = data)
multi_inst_FEDFUNDS <- dynlm(FEDFUNDS ~ L(FEDFUNDS,1:6) + L(UNRATE,0:6) + L(CPI,0:6), data = data)

linearHypothesis(multi_FEDFUNDS, paste0("L(CPI, 1:6)", 1:6, "=0")) # CPI -> FEDFUNDS = yes
linearHypothesis(multi_CPI, paste0("L(FEDFUNDS, 1:6)", 1:6, "=0")) # FEDFUNDS -> CPI = yes
linearHypothesis(multi_inst_FEDFUNDS, "L(CPI, 0:6)0=0") # CPI - FEDFUNDS = no

# UNRATE and FEDFUNDS, control CPI
multi_UNRATE <- dynlm(UNRATE ~ L(UNRATE,1:6) + L(FEDFUNDS,1:6) + L(CPI,1:6), data = data)
multi_FEDFUNDS <- dynlm(FEDFUNDS ~ L(CPI,1:6) + L(FEDFUNDS,1:6) + L(UNRATE,1:6), data = data)
multi_inst_UNRATE <- dynlm(UNRATE ~ L(UNRATE,1:6) + L(FEDFUNDS,0:6) + L(CPI,0:6), data = data)

linearHypothesis(multi_UNRATE, paste0("L(FEDFUNDS, 1:6)", 1:6, "=0")) # FEDFUNDS -> UNRATE = no
linearHypothesis(multi_FEDFUNDS, paste0("L(UNRATE, 1:6)", 1:6, "=0")) # UNRATE -> FEDFUNDS = yes
linearHypothesis(multi_inst_UNRATE, "L(FEDFUNDS, 0:6)0=0") # UNRATE - FEDFUNDS = yes

#Summary:
#There's only one case of a feedback effect in the simple GC tests (CPI <-> FEDFUNDS)
#There's a case of instantaneous relationships (UNRATE - FEDFUNDS) 
