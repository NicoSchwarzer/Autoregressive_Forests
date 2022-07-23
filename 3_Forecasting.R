#####################
## MRF Forecasting ## 
#####################


## This fie does the forecasting. For each Y and each forecasting horizon, 
## the different Macro-RF models and other methods are set-up. 
## The respective predictions and the forecasting errors are derived. Also, the 
## Diebold-Mariano tests are conducted.



setwd("C:/Users/Nico/Documents/Uni/4. Sem/Seminar/Code_data")

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("devtools")) install.packages("devtools")
if (!require("randomForest")) install.packages("randomForest")
if (!require("forecast")) install.packages("forecast")

library(tidyverse)
library(devtools)
library(randomForest)
library(forecast)

# loading the Macro RF package
if (!require("MacroRF")) install_github("philgoucou/macrorf")
library(MacroRF)


source("3b_Functions for Forecasts.R")


#########################
## Monthly forecasting ##
#########################


freq <- "monthly"

#### UNEMP RATE #####

var_name <- "UNEMP_RATE"

# seeting up matrix of RMSE scores to be filled in!
RMSES_ALL <- matrix(0, nrow = 5, ncol = 6)
RMSES_ALL[,1] <- c(1,3,6,9,12)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6]))
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## different forecasting horizons 

hs <- c(1,3,6,9,12) 
counter <- 1

# looping through horizons 

for (h in hs){

  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_monthly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL

  
  ## getting corresponding Forecasts
  set.seed(123)
  FC <- get_forecasts_monthly(St_2, h)
  
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))


  # adding significance starts   
  all_preds <- FC[1][[1]]
  
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds, h)
  

  counter <- counter + 1

  # saving RMSEs
  RMSES_ALL_DF <- data.frame(RMSES_ALL)
  names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
  save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_monthly.csv")
  
  write.csv(RMSES_ALL_DF, save_path, row.names = F)
  
  }



#### CPI_ALL #####

var_name <- "CPI_ALL"

RMSES_ALL <- matrix(0, nrow = 5, ncol = 6)
RMSES_ALL[,1] <- c(1,3,6,9,12)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6]))
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,3,6,9,12) 
counter <- 1 # for counting up 

for (h in hs){
  
  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_monthly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_monthly(St_2, h)
  
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, h, all_preds,h)
  
  counter <- counter + 1

  # saving RMSEs
  RMSES_ALL_DF <- data.frame(RMSES_ALL)
  names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
  save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_monthly.csv")
  write.csv(RMSES_ALL_DF, save_path, row.names = F)
  
  
  }






#### BGS_1yrs_yld #####

var_name <- "BGS_1yrs_yld"

RMSES_ALL <- matrix(0, nrow = 5, ncol = 6)
RMSES_ALL[,1] <- c(1,3,6,9,12)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6]))
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,3,6,9,12) 
counter <- 1 # for counting up 

for (h in hs){
  
  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_monthly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_monthly(St_2, h)
  
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  
  
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds,h)
  
  counter <- counter + 1

  # saving RMSEs
  RMSES_ALL_DF <- data.frame(RMSES_ALL)
  names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
  save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_monthly.csv")
  write.csv(RMSES_ALL_DF, save_path, row.names = F)
  
  }






#### SPREAD #####

var_name <- "SPREAD"

RMSES_ALL <- matrix(0, nrow = 5, ncol = 6)
RMSES_ALL[,1] <- c(1,3,6,9,12)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6]))
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,3,6,9,12) 
counter <- 1 # for counting up 

for (h in hs){
  
  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_monthly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_monthly(St_2, h)
  
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds, h)
  
  counter <- counter + 1

# saving RMSEs
RMSES_ALL_DF <- data.frame(RMSES_ALL)
names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_monthly.csv")
write.csv(RMSES_ALL_DF, save_path, row.names = F)


}





#############################
### Quarterly Forecasting ###
#############################


#### GDP ####

## time -> 2016-04-01"
freq <- "quarterly"

var_name <- "gdp"

RMSES_ALL <- matrix(0, nrow = 3, ncol = 6)
RMSES_ALL[,1] <- c(1,2,4)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6])  )
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,2,4) 
counter <- 1 # for counting up 

for (h in hs){
  
    print(as.character(h))

  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_quarterly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_quarterly(St_2, h)
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds, h)
  
  counter <- counter + 1

# saving RMSEs
RMSES_ALL_DF <- data.frame(RMSES_ALL)
names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_quarterly.csv")
write.csv(RMSES_ALL_DF, save_path, row.names = F)

}



#### UNEMP_RATE ####

var_name <- "UNEMP_RATE"

RMSES_ALL <- matrix(0, nrow = 3, ncol = 6)
RMSES_ALL[,1] <- c(1,2,4)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6])  )
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,2,4) 
counter <- 1 # for counting up 

for (h in hs){
  
  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_quarterly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_quarterly(St_2, h)
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds, h)
  
  
  counter <- counter + 1

# saving RMSEs
RMSES_ALL_DF <- data.frame(RMSES_ALL)
names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_quarterly.csv")
write.csv(RMSES_ALL_DF, save_path, row.names = F)

}



## CPI_ALL

var_name <- "CPI_ALL"

RMSES_ALL <- matrix(0, nrow = 3, ncol = 6)
RMSES_ALL[,1] <- c(1,2,4)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6])  )
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,2,4) 
counter <- 1 # for counting up 


for (h in hs){
  
  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_quarterly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_quarterly(St_2, h)
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds,h)
  
  
  counter <- counter + 1


# saving RMSEs
RMSES_ALL_DF <- data.frame(RMSES_ALL)
names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_quarterly.csv")
write.csv(RMSES_ALL_DF, save_path, row.names = F)

}



#### BGS_1yrs_yld #####

var_name <- "BGS_1yrs_yld"

RMSES_ALL <- matrix(0, nrow = 3, ncol = 6)
RMSES_ALL[,1] <- c(1,2,4)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6])  )
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,2,4) 
counter <- 1 # for counting up 


for (h in hs){
  
  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_quarterly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_quarterly(St_2, h)
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds, h)
  
  counter <- counter + 1
  
  
  # saving RMSEs
  RMSES_ALL_DF <- data.frame(RMSES_ALL)
  names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
  save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_quarterly.csv")
  write.csv(RMSES_ALL_DF, save_path, row.names = F)
  
}





#### SPREAD #####

var_name <- "SPREAD"

RMSES_ALL <- matrix(0, nrow = 3, ncol = 6)
RMSES_ALL[,1] <- c(1,2,4)

RMSES_ALL <- data.frame(as.character(RMSES_ALL[,1]), as.character(RMSES_ALL[,2]), as.character(RMSES_ALL[,3]), as.character(RMSES_ALL[,4]), as.character(RMSES_ALL[,5]), as.character(RMSES_ALL[,6])  )
names(RMSES_ALL) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")

## or different forecasting horizons 

hs <- c(1,2,4) 
counter <- 1 # for counting up 


for (h in hs){
  
  print(as.character(h))
  
  ## reading in ST 
  St <- read.csv(paste0("St_Xt/St_", var_name, "_lags_", as.character(h), "_quarterly.csv"))
  
  names(St)[1:2] <- c("Date", "y")
  St$Date <- as.Date(St$Date)
  St_2 <- St
  St_2$Date <- NULL
  
  ## getting correponding Forecasts
  set.seed(123)
  FC <- get_forecasts_quarterly(St_2, h)
  
  # filling in RMSE Matrix 
  RMSES_ALL[counter,2:6] <- as.character(round(FC[2][[1]], 5))
  
  # adding signifiance starts (if needed)  
  all_preds <- FC[1][[1]]
  all_preds <- data.frame(all_preds)
  names(all_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  RMSES_ALL <- adjust_for_DM_Test(RMSES_ALL, counter, all_preds, h)
  
  counter <- counter + 1
  
  
  # saving RMSEs
  RMSES_ALL_DF <- data.frame(RMSES_ALL)
  names(RMSES_ALL_DF) <- c("H", "AR (4)", "RF", "MAF-RF", "ARRF", "VARRF")
  save_path <- paste0("RMSE/RMSE_", as.character(var_name), "_quarterly.csv")
  write.csv(RMSES_ALL_DF, save_path, row.names = F)
  
}


