###########################################################
## Functions for Variable Importance and surrogate trees ##
###########################################################


if (!require("tidyverse")) install.packages("tidyverse")
if (!require("devtools")) install.packages("devtools")
if (!require("randomForest")) install.packages("randomForest")
if (!require("forecast")) install.packages("forecast")
if (!require("partykit")) install.packages("partykit")
if (!require("lubridate")) install.packages("lubridate")


library(tidyverse)
library(devtools)
library(randomForest)
library(forecast)
library(partykit)
library(lubridate)


# loading the Macro RF package
if (!require("MacroRF")) install_github("philgoucou/macrorf")
library(MacroRF)

setwd("C:/Users/Nico/Documents/Uni/4. Sem/Seminar/Code_data")




### Functions for 0.25 & 0.75 credible intervals
## enable vectorizaation later on 


cred_int_025 <- function(x){
  q1 <- quantile(x, probs = c(0.25), na.rm = T)
  
  return(q1)
}

cred_int_075 <- function(x){
  q1 <- quantile(x, probs = c(0.75), na.rm = T)
  
  return(q1)
}


## Daily to monthly function for the recession data 


daily_to_monthly <- function(input) {
  
  ### This function turns daily data into monthly data ###
  
  #round dates down to week
  input$date <- floor_date(input$date, "month")
  input <- input %>%
    group_by(date) %>%
    summarize(mean = mean(input[,2]))
  
  input$date <- as.character(input$date)
  
  return(data.frame(input))
  
}



## This function trains and fits a surrogate tree on the lags using the 4 most important vars -> take screenshot!
## It also computes the OOS RMSE and the ols counterpart!

surr_tree_ols <- function(St_2, var_imp_1, var_imp_2, var_imp_3, var_imp_4) {
  
  # building the tree
  St_2_fit <- St_2[1:205, ]
  St_2_predict <- St_2[206:nrow(St_2),]
  
  # building up lmtree wiht sup LM parameter stability test with alpha = 0.1
  lin_tree <- lmtree(formula = y ~ lag_1 + lag_2  | var_imp_1 + var_imp_2 + var_imp_3 + var_imp_4,  data <- St_2_fit, alpha = 0.1, bonferroni = F)
  
  print(lin_tree)
  
  preds_oos <- predict(lin_tree, St_2_predict)
  y_vals <- St_2_predict$y
  
  rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
  
  ## calling OLS model - AR(2) constitutes the OLS counterpart 
  model_ar <- lm(y ~ lag_1 + lag_2, data = St_2_fit)
  
  return(list(rmse, model_ar, lin_tree))
  
}


## Counterpart for analsis with quarterly data 

surr_tree_ols_quarterly <- function(St_2, var_imp_1, var_imp_2, var_imp_3, var_imp_4) {
  
  # building the tree
  St_2_fit <- St_2[1:50, ]
  St_2_predict <- St_2[51:nrow(St_2),]
  
  # building up lmtree wiht sup LM parameter stability test with alpha = 0.1
  lin_tree <- lmtree(formula = y ~ lag_1 + lag_2  | var_imp_1 + var_imp_2 + var_imp_3 + var_imp_4,  data <- St_2, alpha = 0.4, bonferroni = F)
  print(lin_tree) # take screenshots - needs to be reconstructed manually!
  
  preds_oos <- predict(lin_tree, St_2_predict)
  y_vals <- St_2_predict$y
  
  rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2)) 
  
  ## calling OLS model - AR(2) constitutes the OLS counterpart 
  model_ar <- lm(y ~ lag_1 + lag_2, data = St_2_fit)
  
  return(list(rmse, model_ar, lin_tree))
  
}




### Function to compute OOB and OOS confidence intervals and
### plot those alongside the MRF- and surrogate parameters

get_cred_ints_and_plots <- function(beta_draws, num_trees, var, h) {
  
  for (i in 1:ncol(beta_draws)) {   # ncol = 3 in this case!
    
    
    betas_here <- beta_draws[, , i][,1]
    
    for (j in 2:num_trees) {
      betas_here <- cbind(betas_here, beta_draws[, , j][,i])
    }
    
    creds_025 <- apply(betas_here, MARGIN = 1, FUN = cred_int_025)
    creds_075 <- apply(betas_here, MARGIN = 1, FUN = cred_int_075)
    
    betas_here <- data.frame(betas[i], creds_025, creds_075)
    names(betas_here) <- c("post_mean", "cr_region_025", "cr_region_075")
    
    if (i == 1){
      betas_here$surrogate_param <- surrogate_intercept
      betas_here$ols_param <- rep( model_ar$coefficients[1], nrow(St_2) )
    } else if (i == 2){
      betas_here$surrogate_param <- surrogate_beta_1
      betas_here$ols_param <- rep( model_ar$coefficients[2], nrow(St_2) )
    } else if (i == 3){
      betas_here$surrogate_param <- surrogate_beta_2
      betas_here$ols_param <- rep( model_ar$coefficients[3], nrow(St_2) )
    } 
    
    
    
    write.csv(betas_here, paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"), row.names = F)
    
    betas_here$date <-  St$Date


#    dfx <- betas_here %>%
#      gather(key = "Method", value = "value", -date) %>%
#      mutate(Method = as.factor(Method)) %>%
#      mutate(date = as.Date(date)  )
    
    
    if (i == 1) {
      ylabel_here <- "Intercept values" 
    } else if (i == 2) {
      ylabel_here <- "Beta 1 values" 
    } else if (i == 3) {
      ylabel_here <- "Beta 2 values" 
    }

      
      ggplot(betas_here) +
      geom_line(aes(as.Date(date), post_mean, color = "ARRF Parameter")) +
      geom_ribbon(aes(x=as.Date(date), ymax=cr_region_025, ymin=cr_region_075), fill="lightblue", alpha=.5) + 
      geom_line(aes(as.Date(date), surrogate_param, color = "Surrogate Parameter"), linetype = "dashed") +
      geom_line(aes(as.Date(date), ols_param, color = "OLS Parameter")) +
      xlab("Date") + 
      scale_color_manual(name='Legend',
                         breaks=c('ARRF Parameter', 'Surrogate Parameter', 'OLS Parameter'),
                         values=c('ARRF Parameter'='blue', 'Surrogate Parameter'='black', 'OLS Parameter' = 'orange')) + 
      geom_vline(xintercept = as.Date("2015-10-01"), linetype="dashed", 
                 color = "red")  + 
      ylab(ylabel_here) + 
      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[1]) ), xmax = as.Date(rec_endings[1]), ymin = -Inf, ymax = Inf,
                        alpha = .27) +  
      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[2]) ), xmax = as.Date(rec_endings[2]), ymin = -Inf, ymax = Inf,
                        alpha = .27) +
      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[3]) ), xmax = as.Date(rec_endings[3]), ymin = -Inf, ymax = Inf,
                        alpha = .27) +  
      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[4]) ), xmax = as.Date(rec_endings[4]), ymin = -Inf, ymax = Inf,
                        alpha = .27) +  
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16 , face="bold",  hjust = 0.5))
    
    if (i == 1) {
      ggsave(paste0("beta_plots/plot", as.character(var), "_intercept_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
    } else if (i == 2) {
      ggsave(paste0("beta_plots/plot", as.character(var), "_beta1_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
    } else if (i == 3) {
      ggsave(paste0("beta_plots/plot", as.character(var), "_beta2_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
    }
    
  }
}



## Counterpart for analsis with quarterly data 

get_cred_ints_and_plots_quarterly <- function(beta_draws, num_trees, var, h) {
  
  for (i in 1:ncol(beta_draws)) {   # ncol = 3 in this case!
    
    betas_here <- beta_draws[, , i][,1]
    
    for (j in 2:num_trees) {
      betas_here <- cbind(betas_here, beta_draws[, , j][,i])
    }
    
    creds_025 <- apply(betas_here, MARGIN = 1, FUN = cred_int_025)
    creds_075 <- apply(betas_here, MARGIN = 1, FUN = cred_int_075)
    
    betas_here <- data.frame(betas[i], creds_025, creds_075)
    names(betas_here) <- c("post_mean", "cr_region_025", "cr_region_075")
    
    if (i == 1){
      betas_here$surrogate_param <- surrogate_intercept
      betas_here$ols_param <- rep( model_ar$coefficients[1], nrow(St_2) )
    } else if (i == 2){
      betas_here$surrogate_param <- surrogate_beta_1
      betas_here$ols_param <- rep( model_ar$coefficients[2], nrow(St_2) )
    } else if (i == 3){
      betas_here$surrogate_param <- surrogate_beta_2
      betas_here$ols_param <- rep( model_ar$coefficients[3], nrow(St_2) )
    } 
    
    
    write.csv(betas_here, paste0("betas/", var, "_beta_", as.character(i), "_H_", as.character(h), ".csv"), row.names = F)
    
    # calling plot 
    betas_here$date <- St$Date
    
    
#    dfx <- betas_here %>%
#      gather(key = "Method", value = "value", -date) %>%
#      mutate(Method = as.factor(Method)) %>%
#      mutate(date = as.Date(date)  )
    
    
    if (i == 1) {
      ylabel_here <- "Intercept values" 
    } else if (i == 2) {
      ylabel_here <- "Beta 1 values" 
    } else if (i == 3) {
      ylabel_here <- "Beta 2 values" 
    }
    
    #plot <-
      ggplot(betas_here) +
      geom_line(aes(as.Date(date), post_mean, color = "ARRF Parameter")) +
      geom_ribbon(aes(x=as.Date(date), ymax=cr_region_025, ymin=cr_region_075), fill="lightblue", alpha=.5) + 
      geom_line(aes(as.Date(date), surrogate_param, color = "Surrogate Parameter"), linetype = "dashed") +
      geom_line(aes(as.Date(date), ols_param, color = "OLS Parameter")) +
      xlab("Date") + 
      scale_color_manual(name='Legend',
                         breaks=c('ARRF Parameter', 'Surrogate Parameter', 'OLS Parameter'),
                         values=c('ARRF Parameter'='blue', 'Surrogate Parameter'='black', 'OLS Parameter' = 'orange')) + 
      geom_vline(xintercept = as.Date("2015-10-01"), linetype="dashed", 
                 color = "red")  + 
      ylab(ylabel_here) + 
#      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[1]) ), xmax = as.Date(rec_endings[1]), ymin = -Inf, ymax = Inf,
#                        alpha = .27) +  
#      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[2]) ), xmax = as.Date(rec_endings[2]), ymin = -Inf, ymax = Inf,
#                        alpha = .27) +
      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[3]) ), xmax = as.Date(rec_endings[3]), ymin = -Inf, ymax = Inf,
                        alpha = .27) +  
      ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[4]) ), xmax = as.Date(rec_endings[4]), ymin = -Inf, ymax = Inf,
                        alpha = .27) +  
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title=element_text(size=14,face="bold")  ,
            axis.text=element_text(size= 13, face="bold"),
            plot.subtitle = element_text(size = 15, hjust = 0.5),
            legend.text = element_text(size=14),
            legend.title = element_text(size=16 , face="bold",  hjust = 0.5))
    
    if (i == 1) {
      ggsave(paste0("beta_plots/plot", as.character(var), "_intercept_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
    } else if (i == 2) {
      ggsave(paste0("beta_plots/plot", as.character(var), "_beta1_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
    } else if (i == 3) {
      ggsave(paste0("beta_plots/plot", as.character(var), "_beta2_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
    }
    
  }
}


#### Functions for Replication File #####




### Function to plot MRF params those alongside the MRF- and surrogate parameters

get_plots_replication <- function(results, i) {
  
  results$date <-  St$Date
  
  if (i == 1) {
    ylabel_here <- "Intercept values" 
  } else if (i == 2) {
    ylabel_here <- "Beta 1 values" 
  } else if (i == 3) {
    ylabel_here <- "Beta 2 values" 
  }
  
  
    ggplot(results) +
    geom_line(aes(as.Date(date), post_mean, color = "ARRF Parameter")) +
    geom_ribbon(aes(x=as.Date(date), ymax=cr_region_025, ymin=cr_region_075), fill="lightblue", alpha=.5) + 
    geom_line(aes(as.Date(date), surrogate_param, color = "Surrogate Parameter"), linetype = "dashed") +
    geom_line(aes(as.Date(date), ols_param, color = "OLS Parameter")) +
    xlab("Date") + 
    scale_color_manual(name='Legend',
                       breaks=c('ARRF Parameter', 'Surrogate Parameter', 'OLS Parameter'),
                       values=c('ARRF Parameter'='blue', 'Surrogate Parameter'='black', 'OLS Parameter' = 'orange')) + 
    geom_vline(xintercept = as.Date("2015-10-01"), linetype="dashed", 
               color = "red")  + 
    ylab(ylabel_here) + 
    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[1]) ), xmax = as.Date(rec_endings[1]), ymin = -Inf, ymax = Inf,
                      alpha = .27) +  
    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[2]) ), xmax = as.Date(rec_endings[2]), ymin = -Inf, ymax = Inf,
                      alpha = .27) +
    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[3]) ), xmax = as.Date(rec_endings[3]), ymin = -Inf, ymax = Inf,
                      alpha = .27) +  
    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[4]) ), xmax = as.Date(rec_endings[4]), ymin = -Inf, ymax = Inf,
                      alpha = .27) +  
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title=element_text(size=14,face="bold")  ,
          axis.text=element_text(size= 13, face="bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16 , face="bold",  hjust = 0.5))
  
  
  if (i == 1) {
    ggsave(paste0("plot", as.character(var), "_intercept_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
  } else if (i == 2) {
    ggsave(paste0("plot", as.character(var), "_beta1_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
  } else if (i == 3) {
    ggsave(paste0("plot", as.character(var), "_beta2_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
  }
  
}




get_plots_replication_quarterly <- function(results, i) {
  
  results$date <-  St$Date
  
  if (i == 1) {
    ylabel_here <- "Intercept values" 
  } else if (i == 2) {
    ylabel_here <- "Beta 1 values" 
  } else if (i == 3) {
    ylabel_here <- "Beta 2 values" 
  }
  
  
    ggplot(results) +
    geom_line(aes(as.Date(date), post_mean, color = "ARRF Parameter")) +
    geom_ribbon(aes(x=as.Date(date), ymax=cr_region_025, ymin=cr_region_075), fill="lightblue", alpha=.5) + 
    geom_line(aes(as.Date(date), surrogate_param, color = "Surrogate Parameter"), linetype = "dashed") +
    geom_line(aes(as.Date(date), ols_param, color = "OLS Parameter")) +
    xlab("Date") + 
    scale_color_manual(name='Legend',
                       breaks=c('ARRF Parameter', 'Surrogate Parameter', 'OLS Parameter'),
                       values=c('ARRF Parameter'='blue', 'Surrogate Parameter'='black', 'OLS Parameter' = 'orange')) + 
    geom_vline(xintercept = as.Date("2015-10-01"), linetype="dashed", 
               color = "red")  + 
    ylab(ylabel_here) + 
    #    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[1]) ), xmax = as.Date(rec_endings[1]), ymin = -Inf, ymax = Inf,
    #                      alpha = .27) +  
    #    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[2]) ), xmax = as.Date(rec_endings[2]), ymin = -Inf, ymax = Inf,
    #                      alpha = .27) +
    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[3]) ), xmax = as.Date(rec_endings[3]), ymin = -Inf, ymax = Inf,
                      alpha = .27) +  
    ggplot2::annotate("rect", xmin = as.Date(as.Date(rec_beginnings[4]) ), xmax = as.Date(rec_endings[4]), ymin = -Inf, ymax = Inf,
                      alpha = .27) +  
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title=element_text(size=14,face="bold")  ,
          axis.text=element_text(size= 13, face="bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16 , face="bold",  hjust = 0.5))
  
  
  if (i == 1) {
    ggsave(paste0("./plot", as.character(var), "_intercept_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
  } else if (i == 2) {
    ggsave(paste0("plot", as.character(var), "_beta1_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
  } else if (i == 3) {
    ggsave(paste0("plot", as.character(var), "_beta2_", as.character(h), "_monthly.png"), height = 6, width = 7*2.5)
  }
  
}


