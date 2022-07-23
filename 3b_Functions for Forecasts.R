####################################
## Functions for Forecasting MRF ###
####################################


## Functions for setting up the MRF models and the other models,
## retrieving their forecast values, RMSE scores and the DM tests

## This function checks for significance of the predictions against the AR (4) predictions using a Diebold-Mariano Test

adjust_for_DM_Test <- function(RMSES_ALL, counter, all_preds, horizon) {
  
  for (i in 3:6){ # from 3 to 6 six first column denotes H and second column is the AR model which is being tested against :)
    
    
    # type of DM M Test with squared residuals in loss differentials ! Also two-sided test!
    
    e1 <- (all_preds$`AR(4)` - all_preds$Observations )^2
    e2 <- (all_preds[,i] - all_preds$Observations )^2
    
    
    g <- dm.test(
      e1,
      e2,
      alternative = "two.sided",
      h = horizon, # forecasting horizon
      power = 2 # squared residuals
    )
    
    if ( g$p.value < 0.005 | g$p.value > 0.995 ) {
      RMSES_ALL[counter, i] <- as.character(paste0(RMSES_ALL[counter, i], "***"))
    } else if ( g$p.value < 0.025 | g$p.value > 0.975 ) {
      RMSES_ALL[counter, i] <- as.character(paste0(RMSES_ALL[counter, i], "**"))
    } else if ( g$p.value < 0.05 | g$p.value > 0.95 ) {
      RMSES_ALL[counter, i] <- as.character(paste0(RMSES_ALL[counter, i], "*"))
    }
  
    
  }
  return(RMSES_ALL)
}  




## This function fits different memthods to the data and calculates the respective RSME Scores and the Predictions

get_forecasts_monthly <- function(St_2, h) {
  
  y_and_preds <- St_2$y[206:nrow(St_2)]
  RMSE_s <- c()
  
  
  ### Autoregression - here some predefined varibles of St come in handy!
  St_2_fit <- St_2[1:205,]
  St_2_predict <- St_2[206:nrow(St_2),]
  names(St_2_fit)[2:5] <- c("lag_1", "lag_2", "lag_3", "lag_4")
  names(St_2_predict)[2:5] <- c("lag_1", "lag_2", "lag_3", "lag_4")
  
  model_ar <- lm(y ~ lag_1 + lag_2, data = St_2_fit)
  
  # gettig OOS values and RMSE
  preds_oos <- predict(model_ar, newdata = St_2_predict)
  y_vals <- St_2_predict$y
  
  rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
  print(paste0("The OOS RMSE for AR is ", as.character(rmse), "!"))
  
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
  
  ### Plain RF
  Xt_2_fit <- St_2[1:205, c(1,6:ncol(St_2))]
  Xt_2_predict <- St_2[206:nrow(St_2),c(1,6:ncol(St_2))]
  
  
  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(206:nrow(St_2)))
  
  trees <- c()
  m_trys <- c()
  
  ntrees  <- c(200,300,600)
  mtry_fracs <- c(round(ncol(St_2)/3.5), round(ncol(St_2)/3), round(ncol(St_2)/2) )
  
  for (a in ntrees) {
    for (b in mtry_fracs) {
      
      trees <- c(trees, a)
      m_trys <- c(m_trys, b)
      
      model1 <- randomForest(y ~ ., data = Xt_2_fit, ntree = a, mtry = b)
      
      # gettig OOS values and RMSE
      preds_oos <- predict(model1, Xt_2_predict)
      y_vals <- Xt_2_predict$y
      
      rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
      
      RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
      preds_pot <- cbind(preds_pot, preds_oos)
      
    }
  }
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  trees_best <- trees[min_rmse_idx]
  fracs_best <- m_trys[min_rmse_idx]
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  
  
  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)

  
  ### RF-MAF 
  St_2_fit <- St_2[1:205, ]
  St_2_predict <- St_2[206:nrow(St_2),]
  
  
  
  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(206:nrow(St_2)))
  
  trees <- c()
  m_trys <- c()
  
  ntrees  <- c(200,300,600)
  mtry_fracs <- c(round(ncol(St_2)/3.5), round(ncol(St_2)/3), round(ncol(St_2)/2) )
  
  for (a in ntrees) {
    for (b in mtry_fracs) {
      
      trees <- c(trees, a)
      m_trys <- c(m_trys, b)
      
      model2 <- randomForest(y ~ ., data = St_2_fit, ntree = a, mtry = b)
      
      # gettig OOS values and RMSE
      preds_oos <- predict(model2, Xt_2_predict)
      y_vals <- St_2_predict$y
      
      rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
      
      RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
      preds_pot <- cbind(preds_pot, preds_oos)
      
    }
  }
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  trees_best <- trees[min_rmse_idx]
  fracs_best <- m_trys[min_rmse_idx]
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  
  
  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)

  
  ## FA-ARRRF
  
  #indices of factors
#  X1 <- paste0("X1_fac_", as.character(h)) # first facor of h lags
#  X2 <- paste0("X2_fac_", as.character(h)) # second factor of h lags
  
#  cols <- 2:116
#  cols1 <- cols[cols != X1]
#  cols2 <- cols1[cols1 != X2]
  
#  mrf.output_fa_arrf =MRF(data=St_2,y.pos=1,x.pos=c(2,3,fac_idx_1,fac_idx_2),S.pos=cols2,mtry.frac=2/3, B=10,oos.pos=206:nrow(St_2), rw.regul = 0.35, ridge.lambda = 0.015, trend.push=4,quantile.rate=0.3, VI = F, keep.forest = F)
  
  #getting OOS Predictions 
#  preds_oos <- mrf.output_fa_arrf$pred
#  y_vals <- St_2$y[206:nrow(St_2)]
  
#  rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
#  print(paste0("The OOS RMSE for FA-ARRF is ", as.character(rmse), "!"))
  
#  y_and_preds <- cbind(y_and_preds, preds_oos)
#  RMSE_s <- c(RMSE_s, rmse)
  
  
  ## ARRF
  
  # positions for specific variables ->2: y, 3:6 -> Regressors, 3:len -> St vars 
  
  ## HP Search 
  
  rw_regus <- seq(0.2, 0.8, by = 0.2)
  ridge_lambdas <- c(0.01, 0.015, 0.02)
  mtry.fracs <- seq(0.3, 0.9, by = 0.3)
  block_sizes <- c(10,20,30)

  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(206:nrow(St_2)))
  
  regus <- c()
  lambdas <- c()
  fracs <- c()
  block_s <- c()

  for (a in rw_regus) {
    for (b in ridge_lambdas) {
       for (d in mtry.fracs) {
          for (e in block_sizes) {
              
              regus <- c(regus, a)
              lambdas <- c(lambdas, b)
              fracs < c(fracs, d)
              block_s <- c(block_s, e)
#             
              print(paste0("Starting with rw_regu: ", as.character(a), " ridge lamdba: ", as.character(b), "fracs: ", as.character(d), " block size: ", as.character(e) ))
              
              mrf.output_arrf = MRF(data=St_2,y.pos=1,x.pos=2:5,S.pos=2:ncol(St_2),mtry.frac=d, subsampling.rate = 0.75, B=100,oos.pos=206:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = F, keep.forest = F)
              
              preds_oos <- mrf.output_arrf$pred
              y_vals <- St_2$y[206:nrow(St_2)]
              
              rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
              RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
              
              preds_pot <- cbind(preds_pot, preds_oos)
              
              print(rmse)
              print(preds_pot)
              
        } 
      }
    }
  }
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  regus_best <- regus[min_rmse_idx]
  lambdas_best <- lambdas[min_rmse_idx]
#  sub_r_best <- sub_rates[min_rmse_idx]
  frac_best <- fracs[min_rmse_idx]
  block_best <- block_s[min_rmse_idx]
  
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  write.csv(preds_oos, paste0("Preds/preds_arrf_", as.character(var_name), "_", as.character(h), "_", as.character(freq), ".csv") )
  
  # saving best hyper-params
  hyper_p_best <- c(regus_best, lambdas_best, frac_best, block_best)
  save_file_hp <- paste0("Best_HP/ARRF_hp_", var_name, "_", as.character(h), "_", as.character(freq), ".csv")

  write.csv(hyper_p_best,file=save_file_hp,row.names=F)
  

  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
  
  
  
  # VARRF
  
  #indices of regressors - depending on if the Y-Variable is one of the potential regressors
  
  regressors_here <- c("UNEMP_RATE_1", "CPI_ALL_1", "SPREAD_1", "BGS_1yrs_yld_1")
  var_here <- paste0(var_name, "_1")
  
  regressors_here <- regressors_here[regressors_here != var_here]

  idx_1 <- which(names(St_2) == regressors_here[1] )
  idx_2 <- which(names(St_2) == regressors_here[2] )
  idx_3 <- which(names(St_2) == regressors_here[3] )
  
  
  ## HP-Search 
  
  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(206:nrow(St_2)))
  
  regus <- c()
  lambdas <- c()
  fracs <- c()
  block_s <- c()
  
  for (a in rw_regus) {
    for (b in ridge_lambdas) {
      for (d in mtry.fracs) {
        for (e in block_sizes) {
          
          regus <- c(regus, a)
          lambdas <- c(lambdas, b)
          fracs < c(fracs, d)
          block_s <- c(block_s, e)
          
          print(paste0("Starting with rw_regu: ", as.character(a), " ridge lamdba: ", as.character(b), "fracs: ", as.character(d), " block size: ", as.character(e) ))
          
          #             
          mrf.output_varrf =MRF(data=St_2,y.pos=1,x.pos=c(2,3, idx_1, idx_2, idx_3),S.pos=2:ncol(St_2),mtry.frac=d,B=100,oos.pos=206:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = F, keep.forest = F)
          
          preds_oos <- mrf.output_varrf$pred
          y_vals <- St_2$y[206:nrow(St_2)]
          
          rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
          RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
          
          preds_pot <- cbind(preds_pot, preds_oos)
          
          print(rmse)
          print(preds_pot)
          
          
        } 
      }
    }
  }
  
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  regus_best <- regus[min_rmse_idx]
  lambdas_best <- lambdas[min_rmse_idx]
#  sub_r_best <- sub_rates[min_rmse_idx]
  frac_best <- fracs[min_rmse_idx]
  block_best <- block_s[min_rmse_idx]
  
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  write.csv(preds_oos, paste0("Preds/preds_varrf_", as.character(var_name), "_", as.character(h), "_", as.character(freq), ".csv") )
  
  
  # saving best hyper-params
  hyper_p_best <- c(regus_best, lambdas_best, frac_best, block_best)
  save_file_hp <- paste0("Best_HP/VARRF_hp_", var_name, "_", as.character(h), "_", as.character(freq), ".csv")
  
  write.csv(hyper_p_best,file=save_file_hp,row.names=F)
  
  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
  
  y_and_preds <- data.frame(y_and_preds)
  names(y_and_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  

  return( list(y_and_preds, RMSE_s))
  
}






### quarterly counterpart 

get_forecasts_quarterly <- function(St_2, h) {
  freq <- "quarterly"
  
  y_and_preds <- St_2$y[51:nrow(St_2)]
  RMSE_s <- c()
  
  
  ### Autoregression - here some predefined variables of St come in handy!
  St_2_fit <- St_2[1:50,]
  St_2_predict <- St_2[51:nrow(St_2),]
  names(St_2_fit)[2:5] <- c("lag_1", "lag_2", "lag_3", "lag_4")
  names(St_2_predict)[2:5] <- c("lag_1", "lag_2", "lag_3", "lag_4")
  
  model_ar <- lm(y ~ lag_1 + lag_2, data = St_2_fit)
  
  # gettig OOS values and RMSE
  preds_oos <- predict(model_ar, newdata = St_2_predict)
  y_vals <- St_2_predict$y
  
  rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
  print(paste0("The OOS RMSE for AR is ", as.character(rmse), "!"))
  
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
  
  ### Plain RF
  Xt_2_fit <- St_2[1:50, c(1,6:ncol(St_2))]
  Xt_2_predict <- St_2[51:nrow(St_2),c(1,6:ncol(St_2))]
  
  
  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(51:nrow(St_2)))
  
  trees <- c()
  m_trys <- c()

  ntrees  <- c(200,300,600)
  mtry_fracs <- c(round(ncol(St_2)/3.5), round(ncol(St_2)/3), round(ncol(St_2)/2) )
  
  for (a in ntrees) {
    for (b in mtry_fracs) {
      
      trees <- c(trees, a)
      m_trys <- c(m_trys, b)
      
      model1 <- randomForest(y ~ ., data = Xt_2_fit, ntree = a, mtry = b)

      # gettig OOS values and RMSE
      preds_oos <- predict(model1, Xt_2_predict)
      y_vals <- Xt_2_predict$y
      
      rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
      
      RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
      preds_pot <- cbind(preds_pot, preds_oos)
       
    }
  }
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  trees_best <- trees[min_rmse_idx]
  fracs_best <- m_trys[min_rmse_idx]
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  
  
  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
  
  
  ### RF-MAF 
  St_2_fit <- St_2[1:50, ]
  St_2_predict <- St_2[51:nrow(St_2),]
  

  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(51:nrow(St_2)))
  
  trees <- c()
  m_trys <- c()
  
  ntrees  <- c(200,300,600)
  mtry_fracs <- c(round(ncol(St_2)/3.5), round(ncol(St_2)/3), round(ncol(St_2)/2) )
  
  for (a in ntrees) {
    for (b in mtry_fracs) {
      
      trees <- c(trees, a)
      m_trys <- c(m_trys, b)
      
      model2 <- randomForest(y ~ ., data = St_2_fit, , ntree = a, mtry = b)
      
      # gettig OOS values and RMSE
      preds_oos <- predict(model2, Xt_2_predict)
      y_vals <- St_2_predict$y
      
      rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
      
      RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
      preds_pot <- cbind(preds_pot, preds_oos)
      
    }
  }
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  trees_best <- trees[min_rmse_idx]
  fracs_best <- m_trys[min_rmse_idx]
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  
  
  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
  
  
  
  ## FA-ARRRF
  
  #indices of factors
  #  X1 <- paste0("X1_fac_", as.character(h)) # first facor of h lags
  #  X2 <- paste0("X2_fac_", as.character(h)) # second factor of h lags
  
  #  cols <- 2:116
  #  cols1 <- cols[cols != X1]
  #  cols2 <- cols1[cols1 != X2]
  
  #  mrf.output_fa_arrf =MRF(data=St_2,y.pos=1,x.pos=c(2,3,fac_idx_1,fac_idx_2),S.pos=cols2,mtry.frac=2/3, B=10,oos.pos=206:nrow(St_2), rw.regul = 0.35, ridge.lambda = 0.015, trend.push=4,quantile.rate=0.3, VI = F, keep.forest = F)
  
  #getting OOS Predictions 
  #  preds_oos <- mrf.output_fa_arrf$pred
  #  y_vals <- St_2$y[206:nrow(St_2)]
  
  #  rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
  #  print(paste0("The OOS RMSE for FA-ARRF is ", as.character(rmse), "!"))
  
  #  y_and_preds <- cbind(y_and_preds, preds_oos)
  #  RMSE_s <- c(RMSE_s, rmse)
  
  
  ## ARRF
  
  # positions for specific variables ->2: y, 3:6 -> Regressors, 3:len -> St vars 
  
  
  ## 51:
  
  ## HP Search 
  
  rw_regus <- seq(0.2, 0.8, by = 0.2)
  ridge_lambdas <- c(0.01, 0.015, 0.02)
  mtry.fracs <- seq(0.3, 0.9, by = 0.3)
  block_sizes <- c(10,15)
  
  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(51:nrow(St_2)))
  
  regus <- c()
  lambdas <- c()
  fracs <- c()
  block_s <- c()
  
  for (a in rw_regus) {
    for (b in ridge_lambdas) {
      for (d in mtry.fracs) {
        for (e in block_sizes) {
          
          regus <- c(regus, a)
          lambdas <- c(lambdas, b)
          fracs < c(fracs, d)
          block_s <- c(block_s, e)
          #             
          print(paste0("Starting with rw_regu: ", as.character(a), " ridge lamdba: ", as.character(b), "fracs: ", as.character(d), " block size: ", as.character(e) ))
          
          mrf.output_arrf = MRF(data=St_2,y.pos=1,x.pos=2:5,S.pos=2:ncol(St_2),mtry.frac=d, subsampling.rate = 0.75, B=100,oos.pos=51:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = F, keep.forest = F)
          
          preds_oos <- mrf.output_arrf$pred
          y_vals <- St_2$y[51:nrow(St_2)]
          
          rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
          RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
          
          preds_pot <- cbind(preds_pot, preds_oos)
          
          print(rmse)
          print(preds_pot)
          
        } 
      }
    }
  }
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  regus_best <- regus[min_rmse_idx]
  lambdas_best <- lambdas[min_rmse_idx]
  frac_best <- fracs[min_rmse_idx]
  block_best <- block_s[min_rmse_idx]
  
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  write.csv(preds_oos, paste0("Preds/preds_arrf_", as.character(var_name), "_", as.character(h), "_", as.character(freq), ".csv") )
  
  # saving best hyper-params
  hyper_p_best <- c(regus_best, lambdas_best, frac_best, block_best)
  save_file_hp <- paste0("Best_HP/ARRF_hp_", var_name, "_", as.character(h), "_", as.character(freq), ".csv")
  
  write.csv(hyper_p_best,file=save_file_hp,row.names=F)
  
  
  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
    
  
  # VARRF
  
  #indices of regressors - depending on if the Y-Variable is one of the potential regressors
  
  regressors_here <- c("GDP_1", "UNEMP_RATE_1", "CPI_ALL_1", "SPREAD_1", "BGS_1yrs_yld_1") # here also considering GDP!
  var_here <- paste0(var_name, "_1")
  
  regressors_here <- regressors_here[regressors_here != var_here]
  
  idx_1 <- which(names(St_2) == regressors_here[1] )
  idx_2 <- which(names(St_2) == regressors_here[2] )
  idx_3 <- which(names(St_2) == regressors_here[3] )
  idx_4 <- which(names(St_2) == regressors_here[4] )
  
  
  ## HP-Search 
  
  RMSEs_hyper_p_search <- c()
  preds_pot <- rep(0,length(51:nrow(St_2)))
  
  regus <- c()
  lambdas <- c()
  fracs <- c()
  block_s <- c()
  
  for (a in rw_regus) {
    for (b in ridge_lambdas) {
      for (d in mtry.fracs) {
        for (e in block_sizes) {
          
          regus <- c(regus, a)
          lambdas <- c(lambdas, b)
          fracs < c(fracs, d)
          block_s <- c(block_s, e)
          
          print(paste0("Starting with rw_regu: ", as.character(a), " ridge lamdba: ", as.character(b), "fracs: ", as.character(d), " block size: ", as.character(e) ))
          
          #             
          mrf.output_varrf =MRF(data=St_2,y.pos=1,x.pos=c(2,3, idx_1, idx_2, idx_3, idx_4),S.pos=2:ncol(St_2),mtry.frac=d,B=100,oos.pos=51:nrow(St_2),rw.regul = a, block.size=e, ridge.lambda = b, trend.push=2,quantile.rate=0.3, VI = F, keep.forest = F)
          
          preds_oos <- mrf.output_varrf$pred
          y_vals <- St_2$y[51:nrow(St_2)]
          
          rmse <- sqrt(1/length(y_vals)*sum((y_vals - preds_oos)^2))
          RMSEs_hyper_p_search <- c(RMSEs_hyper_p_search, rmse)
          
          preds_pot <- cbind(preds_pot, preds_oos)
          
          print(rmse)
          print(preds_pot)
          
          
        } 
      }
    }
  }
  
  
  rmse <- min(RMSEs_hyper_p_search)
  min_rmse_idx <- which(RMSEs_hyper_p_search == rmse)
  
  regus_best <- regus[min_rmse_idx]
  lambdas_best <- lambdas[min_rmse_idx]
  frac_best <- fracs[min_rmse_idx]
  block_best <- block_s[min_rmse_idx]
  
  preds_oos <- preds_pot[,(min_rmse_idx+1)]
  write.csv(preds_oos, paste0("Preds/preds_varrf_", as.character(var_name), "_", as.character(h), "_", as.character(freq), ".csv") )
  
  
  # saving best hyper-params
  hyper_p_best <- c(regus_best, lambdas_best, frac_best, block_best)
  save_file_hp <- paste0("Best_HP/VARRF_hp_", var_name, "_", as.character(h), "_", as.character(freq), ".csv")
  
  write.csv(hyper_p_best,file=save_file_hp,row.names=F)
  
  # saving beta and rmse 
  y_and_preds <- cbind(y_and_preds, preds_oos)
  RMSE_s <- c(RMSE_s, rmse)
  
  #y_and_preds <- data.frame(y_and_preds)
  #names(y_and_preds) <- c("Observations", "AR(4)", "RF", "MRF-MAF", "ARRF", "VARRF")
  
  
  
  return( list(y_and_preds, RMSE_s))
  
}
