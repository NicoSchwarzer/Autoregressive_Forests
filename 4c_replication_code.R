##################################################
### VIs and surrogate trees -- Replication File ##
##################################################

## This file conducts all steps in the file "4_VI_lin_trees" without the MRF calculation.
## In case of replication, this file saves a great amount of time!

## File "4_VI_lin_trees" re-computes the MRF ARRF model with 200 trees and retrieves the VI scores
## Based on these, the linear surrogate trees are set up. For these, the significance values 
## are retrieved. Lastly, comparison plots of the ARRF GTVP, the parameters of the trees 
## and the OLS parameters are created.


if (!require("tidyverse")) install.packages("tidyverse")
if (!require("devtools")) install.packages("devtools")
if (!require("randomForest")) install.packages("randomForest")
if (!require("forecast")) install.packages("forecast")
if (!require("partykit")) install.packages("partykit")


library(tidyverse)
library(devtools)
library(randomForest)
library(forecast)
library(partykit)

# loading the Macro RF package
if (!require("MacroRF")) install_github("philgoucou/macrorf")
library(MacroRF)


setwd("C:/Users/Nico/Documents/Uni/4. Sem/Seminar/Code_data")

# sourcing in funcions - also replication functions!
source("4b_functions_VI_lin_trees.R")




## Getting (binary) recession data for the plots ## 

rec <- "GBRRECDM"
REC <- alfred::get_fred_series(rec)
REC <- REC %>% drop_na()
names(REC) <- c("date", "rec")

REC$date <- lubridate::floor_date(REC$date, "month") # to monthly from daily
REC <- aggregate(REC[, 2], list(REC$date), mean)
names(REC) <- c("date", "rec")
REC$date <- as.Date(REC$date)


## limiting ti sued time horizon
REC <- REC[REC$date >= as.Date("1998-08-01"),]      # starting one quarter prior, see reason below!       
REC <- REC[REC$date <= as.Date("2021-07-01")  ,]


## important: these are time times that a recession began/ended, yet in the final plots,
## t refers to the time of Y, hence lagged variables by one quarter is necessary

one_month_lags <- lag(REC$rec, 3, order_by = REC$date)
REC$rec <- one_month_lags

# deleting first row
REC <- REC[4:nrow(REC),]

rec_endings <- as.Date(c() ) 
rec_beginnings <- as.Date(c(REC$date[1])) ## # recession in the beginning!

for (i in 2:nrow(REC)) {
  if (REC$rec[i] == 0 & REC$rec[i-1] == 1 ) {
    rec_endings <- c(rec_endings, REC$date[i-1])
  } else if (REC$rec[i] == 1 & REC$rec[i-1] == 0 ) {
    rec_beginnings <- c(rec_beginnings, REC$date[i-1])
  }
}

# IMPORTANT: The values of rec_beginnings and rec_endings shall be incorporated in the plot in the 'get_cred_ints_and_plots' function!



## Because of high computational costs when running MRF with VI and grid search, VARRF with optimal HP are retrained here!
num_trees <- 200 # 200


##################
### UNEMP Rate ###
##################

var <- "UNEMP_RATE"
h <- 3
freq <- "monthly"

St <- read.csv(paste0("St_Xt/St_", var, "_lags_", as.character(h), "_monthly.csv"))

names(St)[1:2] <- c("Date", "y")
St$Date <- as.Date(St$Date)
St_2 <- St
St_2$Date <- NULL

# getting best HP - not really needed anymore!
#HP_path <- paste0("Best_HP/ARRF_hp_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#HPs <- read.csv(HP_path)

#a <- HPs[1,1] 
#b <- HPs[2,1]
#d <- HPs[3,1]
#e <- HPs[4,1]

# retraining WTITH VARIABLE IMPORTANCE (VI = T) - step skipped
#set.seed(123)
# mrf.output_arrf = MRF(data=St_2,y.pos=1,x.pos=2:3,S.pos=2:ncol(St_2),mtry.frac=d, subsampling.rate = 0.75, B=num_trees,oos.pos=206:nrow(St_2), rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = T, keep.forest = T)

# most important 4 variables
#name_1 <- names(mrf.output_arrf$important.S)[1] # VAC_TOT 4	UK Vacancies (thousands) - Total  --> macht mega sinn :) !
#name_2 <- names(mrf.output_arrf$important.S)[2] # #  VAC tot 3
#name_3 <- names(mrf.output_arrf$important.S)[3] # 3 AVG_WEEK_HRS_FULL	Average actual weekly hours of work for full-time workers (seasonally adjusted)
#name_4 <- names(mrf.output_arrf$important.S)[4] #  3  -IMP_MACH	Trade in Goods: Machinery and Transport (7): WW: Imports: BOP: CVM: SA  in million pounds

#path_save_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#write.csv(data.frame(name_1, name_2, name_3, name_4), path_save_imp_vars, row.names = F)


# intercept results 
i <- 1 
results_mrf_alpha <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]


# beta 1 results 
i <- 2 
results_mrf_beta_1 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]


# beta 2 results 
i <- 3 
results_mrf_beta_2 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]



### examining Betas and building surrogate tree ###

## surrogate tree ##

path_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
imp_vars <- read.csv(path_imp_vars)[1,]

name_1 <- imp_vars$name_1
name_2 <- imp_vars$name_2
name_3 <- imp_vars$name_3
name_4 <- imp_vars$name_4

names(St_2)[2:3] <- c("lag_1", "lag_2")

names(St_2)[ which(names(St_2) == name_1) ] <- "var_imp_1"
names(St_2)[ which(names(St_2) == name_2) ] <- "var_imp_2"
names(St_2)[ which(names(St_2) == name_3) ] <- "var_imp_3"
names(St_2)[ which(names(St_2) == name_4) ] <- "var_imp_4"


output <- surr_tree_ols(St_2, var_imp_1, var_imp_2, var_imp_3, var_imp_4)

print(paste0("The OOS RMSE for the surrogate tree is ", as.character(output[1]) ))  ## 0.0854102
model_ar <- output[2][[1]]

## significances

summary(output[3][[1]][2])

summary(output[3][[1]][4])

summary(output[3][[1]][5])




# getting corresponding params - fill in manually :/ 
surrogate_intercept <- c()
surrogate_beta_1 <- c()
surrogate_beta_2 <- c()

for (i in 1:nrow(St_2)) {
  
  if (St_2$var_imp_1[i] <= -0.01613){ # 30
    surrogate_intercept <- c(surrogate_intercept,  0.06508189  ) # ***
    surrogate_beta_1 <- c(surrogate_beta_1,0.16637420 )
    surrogate_beta_2 <- c(surrogate_beta_2,0.15039848 )
  } else { # # 245
    if (St_2$var_imp_3[i] <= -0.00931) { # 30
      surrogate_intercept <- c(surrogate_intercept,         0.004881953   ) 
      surrogate_beta_1 <- c(surrogate_beta_1, -0.417366947   )
      surrogate_beta_2 <- c(surrogate_beta_2,0.234493798 )
    } else {  # 215
      surrogate_intercept <- c(surrogate_intercept, -0.02878254    )   # *** 
      surrogate_beta_1 <- c(surrogate_beta_1, 0.18895991     ) # **
      surrogate_beta_2 <- c(surrogate_beta_2,0.08805632  )
    }
  }
}


# get posterior means and OOB / OOS betas
#betas <- data.frame(mrf.output_arrf$betas) ## (posterior) means
#beta_draws <- mrf.output_arrf$betas.draws ## all oob/oos betas



for (i in 1:3) {
  
  if (i == 1) {
    results <- results_mrf_alpha
    results$surrogate_param <- surrogate_intercept
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
    
    
  } else if (i == 2) {
    results <-results_mrf_beta_1
    results$surrogate_param <-surrogate_beta_1
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )

    
    }  else if (i == 3) {
    results <-results_mrf_beta_2
    results$surrogate_param <-surrogate_beta_2
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
  
    
    }
  
  get_plots_replication(results, i )
  
}


#####################
### CPI Inflation ###
#####################

var <- "CPI_ALL"
h <- 3
freq <- "monthly"

St <- read.csv(paste0("St_Xt/St_", var, "_lags_", as.character(h), "_monthly.csv"))

names(St)[1:2] <- c("Date", "y")
St$Date <- as.Date(St$Date)
St_2 <- St
St_2$Date <- NULL


#HP_path <- paste0("Best_HP/ARRF_hp_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#HPs <- read.csv(HP_path)

#a <- HPs[1,1] 
#b <- HPs[2,1]
#d <- HPs[3,1]
#e <- HPs[4,1]

#set.seed(123)
#mrf.output_arrf2 = MRF(data=St_2,y.pos=1,x.pos=2:3,S.pos=2:ncol(St_2),mtry.frac=d, subsampling.rate = 0.75, B=num_trees,oos.pos=206:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = T, keep.forest = T)

# most important 4 variables cpi_all
#name_1 <- names(mrf.output_arrf2$important.S)[1] # BCI_3 - business confidence index
#name_2 <- names(mrf.output_arrf2$important.S)[2]  # EXP_CRUDE_MAT	Trade in Goods: Crude Materials (2): WW: Exports: BOP: CVM: SA  in million pounds
#name_3 <- names(mrf.output_arrf2$important.S)[3] # maf_pc_1 
#name_4 <- names(mrf.output_arrf2$important.S)[4] # Lag_CPI_ALL_4

# IMP_MACH	Trade in Goods: Machinery and Transport (7): WW: Imports: BOP: CVM: SA  in million pounds

#path_save_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#write.csv(data.frame(name_1, name_2, name_3, name_4), path_save_imp_vars, row.names = F)



# intercept results 
i <- 1 
results_mrf_alpha <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 1 results 
i <- 2 
results_mrf_beta_1 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 2 results 
i <- 3 
results_mrf_beta_2 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]


### examining Betas and building surrogate tree ###

## surrogate tree ##

path_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
imp_vars <- read.csv(path_imp_vars)[1,]

name_1 <- imp_vars$name_1
name_2 <- imp_vars$name_2
name_3 <-  imp_vars$name_3
name_4 <- imp_vars$name_4

names(St_2)[2:3] <- c("lag_1", "lag_2")

names(St_2)[ which(names(St_2) == name_1) ] <- "var_imp_1"
names(St_2)[ which(names(St_2) == name_2) ] <- "var_imp_2"
names(St_2)[ which(names(St_2) == name_3) ] <- "var_imp_3"

# the var mp 4 assignement will nto work here as a lagged varaible is the variale to split by 
# hence calling the surr_tree_ols function wont't work

St_2_fit <- St_2[1:205, ]
St_2_predict <- St_2[206:nrow(St_2),]

lin_tree <- lmtree(y ~ lag_1 + lag_2  | var_imp_1 + var_imp_2 + var_imp_3 + lag_2,  data <- St_2_fit, alpha = 0.39, bonferroni = F) # should output a structure - take screenshot
print(lin_tree)

preds_oos <- predict(lin_tree, St_2_predict)
y_vals <- St_2_predict$y

rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))

## calling OLS model - AR(2) constitutes the OLS counterpart 
model_ar <- lm(y ~ lag_1 + lag_2, data = St_2_fit)


# examining significances


# getting corresponding params - fill in manually :/ 
surrogate_intercept <- c()
surrogate_beta_1 <- c()
surrogate_beta_2 <- c()




for (i in 1:nrow(St_2)) {
  
  if (St_2$var_imp_1[i] <= -0.31439){ # 33
    surrogate_intercept <- c(surrogate_intercept, 0.001629139   ) # ***
    surrogate_beta_1 <- c(surrogate_beta_1, -0.089709671   )
    surrogate_beta_2 <- c(surrogate_beta_2, -0.121212491   )
  } else if (St_2$var_imp_3[i] <= 0.48427 ) { # 118
    surrogate_intercept <- c(surrogate_intercept,  0.001128211      ) # ***
    surrogate_beta_1 <- c(surrogate_beta_1, 0.048555886   ) # **
    surrogate_beta_2 <- c(surrogate_beta_2, 0.037317447  )
  } else { # 54
    surrogate_intercept <- c(surrogate_intercept,  0.001812394       ) ## ***
    surrogate_beta_1 <- c(surrogate_beta_1, 0.186276050   ) # **
    surrogate_beta_2 <- c(surrogate_beta_2, 0.162522575   )
    
  }
}



# get posterior means and OOB / OOS betas - not needed 
#betas <- data.frame(mrf.output_arrf2$betas) ## (posterior) means
#beta_draws <- mrf.output_arrf2$betas.draws ## all oob/oos betas


# plotting and saving parameter comparison plots 

for (i in 1:3) {
  
  if (i == 1) {
    results <- results_mrf_alpha
    results$surrogate_param <- surrogate_intercept
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
  } else if (i == 2) {
    results <-results_mrf_beta_1
    results$surrogate_param <-surrogate_beta_1
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
  }  else if (i == 3) {
    results <-results_mrf_beta_2
    results$surrogate_param <-surrogate_beta_2
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
  }
  
  get_plots_replication(results, i )
  
}





### Function to plot MRF params those alongside the MRF- and surrogate parameters




#############################
### 1 Year GVT Bond yield ###
#############################


var <- "BGS_1yrs_yld"
h <- 3
freq <- "monthly"

St <- read.csv(paste0("St_Xt/St_", var, "_lags_", as.character(h), "_monthly.csv"))

names(St)[1:2] <- c("Date", "y")
St$Date <- as.Date(St$Date)
St_2 <- St
St_2$Date <- NULL


#HP_path <- paste0("Best_HP/ARRF_hp_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#HPs <- read.csv(HP_path)

#a <- HPs[1,1] 
#b <- HPs[2,1]
#d <- HPs[3,1]
#e <- HPs[4,1]

#set.seed(123)
#mrf.output_arrf3 = MRF(data=St_2,y.pos=1,x.pos=2:3,S.pos=2:ncol(St_2),mtry.frac=d, subsampling.rate = 0.75, B=num_trees,oos.pos=206:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = T, keep.forest = T)


# most important 4 variables cpi_all
#name_1 <-   names(mrf.output_arrf3$important.S)[1] ## cpi all 3 
#name_2 <- names(mrf.output_arrf3$important.S)[2] ##  bci 3
#name_3 <- names(mrf.output_arrf3$important.S)[3] ## m2_4
#name_4 <- names(mrf.output_arrf3$important.S)[4]  ## bank rate 3 

# IMP_MACH	Trade in Goods: Machinery and Transport (7): WW: Imports: BOP: CVM: SA  in million pounds

#path_save_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#write.csv(data.frame(name_1, name_2, name_3, name_4), path_save_imp_vars, row.names = F)


# intercept results 
i <- 1 
results_mrf_alpha <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 1 results 
i <- 2 
results_mrf_beta_1 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 2 results 
i <- 3 
results_mrf_beta_2 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]




### examining Betas and building surrogate tree ###

## surrogate tree ##

path_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
imp_vars <- read.csv(path_imp_vars)[1,]

name_1 <- imp_vars$name_1
name_2 <- imp_vars$name_2
name_3 <- imp_vars$name_3
name_4 <- imp_vars$name_4

names(St_2)[2:3] <- c("lag_1", "lag_2")

names(St_2)[ which(names(St_2) == name_1) ] <- "var_imp_1"
names(St_2)[ which(names(St_2) == name_2) ] <- "var_imp_2"
names(St_2)[ which(names(St_2) == name_3) ] <- "var_imp_3"
names(St_2)[ which(names(St_2) == name_4) ] <- "var_imp_4"


output <- surr_tree_ols(St_2, var_imp_1, var_imp_2, var_imp_3, var_imp_4)


# examining significances 
summary(  output[3][[1]][[1]][2] ) # first split 
summary(  output[3][[1]][[1]][3] ) # second split 

print(paste0("The OOS RMSE for the surrogate tree is ", as.character(output[1]) ))  ## 0.111762
model_ar <- output[2][[1]]


# getting corresponding params - fill in manually :/ 
surrogate_intercept <- c()
surrogate_beta_1 <- c()
surrogate_beta_2 <- c()


## 145 vs 60 


for (i in 1:nrow(St_2)) {
  
  if (St_2$var_imp_1[i] <= 0.00342){
    surrogate_intercept <- c(surrogate_intercept, 0.011566927  ) 
    surrogate_beta_1 <- c(surrogate_beta_1, 0.142918739  ) # *
    surrogate_beta_2 <- c(surrogate_beta_2, 0.004855697  )
  } else {
    surrogate_intercept <- c(surrogate_intercept, 0.07416844     ) 
    surrogate_beta_1 <- c(surrogate_beta_1, 0.36619576    ) # *
    surrogate_beta_2 <- c(surrogate_beta_2,  -0.87477563  )   # ***
  } 
}


# get posterior means and OOB / OOS betas
#betas <- data.frame(mrf.output_arrf3$betas) ## (posterior) means
#beta_draws <- mrf.output_arrf3$betas.draws ## all oob/oos betas




# plotting and saving parameter comparison plots 

for (i in 1:3) {
  
  if (i == 1) {
    results <- results_mrf_alpha
    results$surrogate_param <- surrogate_intercept
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )

    
  } else if (i == 2) {
    results <-results_mrf_beta_1
    results$surrogate_param <-surrogate_beta_1
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )

  }  else if (i == 3) {
    results <-results_mrf_beta_2
    results$surrogate_param <-surrogate_beta_2
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
    
  }
  
  get_plots_replication(results, i )
  
}



############
## SPREAD ##
############

var <- "SPREAD"
h <- 3
freq <- "monthly"

St <- read.csv(paste0("St_Xt/St_", var, "_lags_", as.character(h), "_monthly.csv"))

names(St)[1:2] <- c("Date", "y")
St$Date <- as.Date(St$Date)
St_2 <- St
St_2$Date <- NULL


#HP_path <- paste0("Best_HP/ARRF_hp_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#HPs <- read.csv(HP_path)

#a <- HPs[1,1] 
#b <- HPs[2,1]
#d <- HPs[3,1]
#e <- HPs[4,1]

#set.seed(123)
#mrf.output_arrf4 = MRF(data=St_2,y.pos=1,x.pos=2:3,S.pos=2:ncol(St_2),mtry.frac=d, subsampling.rate = 0.75, B=num_trees,oos.pos=206:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = T, keep.forest = T)


# most important 4 variables cpi_all
#name_1 <- names(mrf.output_arrf4$important.S)[1] ## "GBP_US_3"
#name_2 <- names(mrf.output_arrf4$important.S)[2] ## "LIBOR_3mth_4" -> close to bank rate 
#name_3 <- names(mrf.output_arrf4$important.S)[3] ## cci 4 -> Consumer confidence index (CCI)Amplitude adjusted, Long-term average = 100
#name_4 <- names(mrf.output_arrf4$important.S)[4] ## Total actual weekly hours worked (millions): UK: All: SA  - 3

#path_save_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#write.csv(data.frame(name_1, name_2, name_3, name_4), path_save_imp_vars, row.names = F)



# intercept results 
i <- 1 
results_mrf_alpha <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 1 results 
i <- 2 
results_mrf_beta_1 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 2 results 
i <- 3 
results_mrf_beta_2 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]


### examining Betas and building surrogate tree ###

## surrogate tree ##

path_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
imp_vars <- read.csv(path_imp_vars)[1,]


name_1 <- imp_vars$name_1
name_2 <- imp_vars$name_2
name_3 <- imp_vars$name_3
name_4 <- imp_vars$name_4

names(St_2)[2:3] <- c("lag_1", "lag_2")

names(St_2)[ which(names(St_2) == name_1) ] <- "var_imp_1"
names(St_2)[ which(names(St_2) == name_2) ] <- "var_imp_2"
names(St_2)[ which(names(St_2) == name_3) ] <- "var_imp_3"
names(St_2)[ which(names(St_2) == name_4) ] <- "var_imp_4"


output <- surr_tree_ols(St_2, var_imp_1, var_imp_2, var_imp_3, var_imp_4)

# examining significances 
summary(  output[3][[1]][[1]][2] ) # first split 
summary(  output[3][[1]][[1]][3] ) # second split 

print(paste0("The OOS RMSE for the surrogate tree is ", as.character(output[1]) ))  ## 0.144331109580423
model_ar <- output[2][[1]]



# getting corresponding params - fill in manually :/ 
surrogate_intercept <- c()
surrogate_beta_1 <- c()
surrogate_beta_2 <- c()


for (i in 1:nrow(St_2)) {
  
  if (St_2$var_imp_2[i] <= -0.0972){
    surrogate_intercept <- c(surrogate_intercept, 0.1894  ) # *** 
    surrogate_beta_1 <- c(surrogate_beta_1,-0.1884  )
    surrogate_beta_2 <- c(surrogate_beta_2,  0.0859  )
  } else {
    surrogate_intercept <- c(surrogate_intercept, -0.0171     ) 
    surrogate_beta_1 <- c(surrogate_beta_1, 0.119   )
    surrogate_beta_2 <- c(surrogate_beta_2, -0.0960   )
  } 
}


# get posterior means and OOB / OOS betas
#betas <- data.frame(mrf.output_arrf4$betas) ## (posterior) means
#beta_draws <- mrf.output_arrf4$betas.draws ## all oob/oos betas


# plotting and saving parameter comparison plots 

for (i in 1:3) {
  
  if (i == 1) {
    results <- results_mrf_alpha
    results$surrogate_param <- surrogate_intercept
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
  } else if (i == 2) {
    results <-results_mrf_beta_1
    results$surrogate_param <-surrogate_beta_1
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
  }  else if (i == 3) {
    results <-results_mrf_beta_2
    results$surrogate_param <-surrogate_beta_2
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )
  }
  
  get_plots_replication(results, i )
  
}






##################
## Quarerly GDP ##
##################

var <- "gdp"
h <- 1
freq <- "quarterly"

St <- read.csv(paste0("St_Xt/St_", var, "_lags_", as.character(h), "_quarterly.csv"))

names(St)[1:2] <- c("Date", "y")
St$Date <- as.Date(St$Date)
St_2 <- St
St_2$Date <- NULL


#HP_path <- paste0("Best_HP/ARRF_hp_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#HPs <- read.csv(HP_path)

#a <- HPs[1,1] 
#b <- HPs[2,1]
#d <- HPs[3,1]
#e <- HPs[4,1]

#set.seed(123)
#mrf.output_arrf5 = MRF(data=St_2,y.pos=1,x.pos=2:3,S.pos=2:ncol(St_2),mtry.frac=d, subsampling.rate = 0.75, B=num_trees,oos.pos=51:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = T, keep.forest = T)

# most important 4 variables cpi_all
#name_1 <- names(mrf.output_arrf5$important.S)[1] ## "LIBOR_3mth_2"
#name_2 <- names(mrf.output_arrf5$important.S)[2] ## bci 2 
#name_3 <- names(mrf.output_arrf5$important.S)[3] ##  "UNEMP_DURA_12mth._1"
#name_4 <- names(mrf.output_arrf5$important.S)[4] ## IOP dur 1

#path_save_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
#write.csv(data.frame(name_1, name_2, name_3, name_4), path_save_imp_vars, row.names = F)



# intercept results 
i <- 1 
results_mrf_alpha <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 1 results 
i <- 2 
results_mrf_beta_1 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]

# beta 2 results 
i <- 3 
results_mrf_beta_2 <- read.csv(paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"))[,c("post_mean", "cr_region_025" , "cr_region_075")]



### examining Betas and building surrogate tree ###

## surrogate tree ##

path_imp_vars <- paste0("IMP_VARS/vars_", as.character(var), "_", as.character(h), "_", as.character(freq), ".csv")
imp_vars <- read.csv(path_imp_vars)[1,]


name_1 <- imp_vars$name_1
name_2 <- imp_vars$name_2
name_3 <- imp_vars$name_3
name_4 <- imp_vars$name_4

names(St_2)[2:3] <- c("lag_1", "lag_2")

names(St_2)[ which(names(St_2) == name_1) ] <- "var_imp_1"
names(St_2)[ which(names(St_2) == name_2) ] <- "var_imp_2"
names(St_2)[ which(names(St_2) == name_3) ] <- "var_imp_3"
names(St_2)[ which(names(St_2) == name_4) ] <- "var_imp_4"


output <- surr_tree_ols_quarterly(St_2, var_imp_1, var_imp_2, var_imp_3, var_imp_4)


# examining significances 
summary(  output[3][[1]][[1]][2] ) # first split 
summary(  output[3][[1]][[1]][3] ) # second split 


print(paste0("The OOS RMSE for the surrogate tree is ", as.character(output[1]) ))  ## 0.0806
model_ar <- output[2][[1]]


# getting corresponding params - fill in manually :/ 
surrogate_intercept <- c()
surrogate_beta_1 <- c()
surrogate_beta_2 <- c()

## 30 vs 38 

for (i in 1:nrow(St_2)) {
  
  if (St_2$var_imp_2[i] <= -0.04961){
    surrogate_intercept <- c(surrogate_intercept,  -0.003873726  )  
    surrogate_beta_1 <- c(surrogate_beta_1, 3.887877180  ) # ***
    surrogate_beta_2 <- c(surrogate_beta_2,  -2.330494440   ) # ***
  } else {
    surrogate_intercept <- c(surrogate_intercept, 0.007794824    ) # *** 
    surrogate_beta_1 <- c(surrogate_beta_1, -0.679927465   ) # ***
    surrogate_beta_2 <- c(surrogate_beta_2, 0.047945713    )
  } 
}


# get posterior means and OOB / OOS betas
#betas <- data.frame(mrf.output_arrf5$betas) ## (posterior) means
#beta_draws <- mrf.output_arrf5$betas.draws ## all oob/oos betas


# plotting and saving parameter comparison plots 

for (i in 1:3) {
  
  if (i == 1) {
    results <- results_mrf_alpha
    results$surrogate_param <- surrogate_intercept
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )

  } else if (i == 2) {
    results <-results_mrf_beta_1
    results$surrogate_param <-surrogate_beta_1
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )

  }  else if (i == 3) {
    results <-results_mrf_beta_2
    results$surrogate_param <-surrogate_beta_2
    results$ols_param <- rep( model_ar$coefficients[i], nrow(St_2) )

  }
  
  get_plots_replication_quarterly(results, i )
  
}











