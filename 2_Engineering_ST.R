##################
## Preparing St ##
##################

## This file 'engineers' the St and the Xt (input to the RF Part in MRF) as suggested by Coulombe (2020)
## See Chapter "Engineering MRF Input Data"


setwd("C:/Users/Nico/Documents/Uni/4. Sem/Seminar/Code_data")


if (!require("tidyverse")) install.packages("tidyverse")
if (!require("psych")) install.packages("psych")

library(tidyverse)
library(psych)


# sourcing in relevant functions 
# Importantly, most of the code is to be found in the sourced-in functions!
source("2b_function_for_St.R")


### Setting variables for lags etc. for the creation of the respective Sts
# when ignoring some parts in the creation of St, these will simply have no effect :) 


num_y_lags  <- 4  # number of lags of y
num_lags_y <- num_y_lags
num_other_lags <- 2 # number of lags of other data than y
num_fac <- 5 # number of factors (cross-sectional principal components) of other data than y
num_lag_fac <- 2  # lags of these factors
num_maf <- 2 # number of moving average factors (PC of lags of y)
num_maf_lags <- 8 # number of lags for the latter



### Important to understand:

# Note that even for a forecasting lag of one, for the MAF, one series was lagged 8 times.
# Hence, the first 8 rows must be deleted since these contain NaNs. This increases with higher lags! 


# For each combination of dependent variable, frequency and foreasting horizon, one csv will be saved!
# The names of the lags for h=1 refer to the lags behind the dependent variable in T (e.g. "XXX_1") 
# For h>2, these are changes correspondingly.


#### Engineering Monthly St ####

df <- read.csv("base_data_uk_stat.csv")
names(df)[1] <- "Date"

freq <- "monthly"

### UNEMP_RATE

var <- "UNEMP_RATE"
create_different_St(df, var, freq, 12)





### CPI_ALL

var <- "CPI_ALL"
create_different_St(df, var, freq, 12)

### 1 Year Yield Gvt Bonds


var <- "BGS_1yrs_yld"
create_different_St(df, var, freq, 12)


#df$BGS_1yrs_yld


### SPREAD

var <- "SPREAD"
create_different_St(df, var, freq, 12)



### Engineering Quarterly St ### 

df <- read.csv("all_data_quarterly.csv")
names(df)[1] <- "Date"

freq <- "quarterly"


### GDP

var <- "gdp"
create_different_St(df, var, freq, 4)


### UNEMP_RATE

var <- "UNEMP_RATE"
create_different_St(df, var, freq, 4)

### CPI_ALL

var <- "CPI_ALL"
create_different_St(df, var, freq, 4)

### 1 Year Yield Gvt Bonds

var <- "BGS_1yrs_yld"
create_different_St(df, var, freq, 4)


### SPREAD

var <- "SPREAD"
create_different_St(df, var, freq, 4)
