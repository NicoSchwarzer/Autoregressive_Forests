######################
## Functions for St ##
######################

### This file contains all function necessary for the engineering of St

### Simple functions (needed to be defined here, else vectorizing in complex functions won't work)


lagging <- function(x, num_lag) {
  lagged <- c(rep(NA,num_lag), head(x, -num_lag))
  return(lagged)
}



lagging_1 <- function(x){
  l <- lagging(x,1)
  return(l)
}

lagging_2 <- function(x){
  l <- lagging(x,2)
  return(l)
}



add_1 <- function(x) {
  x <- paste0(x, "_1")
  return(x)
}

add_2 <- function(x) {
  x <- paste0(x, "_2")
  return(x)
}




### Function to engineer St like in coulombe (2020), following args:
## importnantly, this is for a forecasting horizon of h = 1
## DF - obvious
## var - dependent variable
## num_lags_y - num of lags of y
## num_other_lags - number of lags of other variables (max 2)
## num_fac - number of factors of other variables
## num_lag_fac - number of lags of these factors  (max 2)
## num_maf - number of moving avergae factors
## num_maf_lags - number of lags to consider for moving avergae factos
## include_own_lags - Bool: Whether to include own lags at all
## include_fac - Bool: Whether to include other factors at all
## include_maf - Bool: Whether to include moving vergae factors at all


engineer_st <- function(df, var, num_lags_y, num_other_lags, num_fac, num_lag_fac, num_maf, num_maf_lags,  include_own_lags, include_fac, include_maf) { 
  
  var_idx <- which(names(df) == var)
  St <- data.frame(df$Date, df[, var_idx])
  
  
  ## own lags 
  if (include_own_lags == T) {
    for (i in 1:num_lags_y) {
      lag_y <- lagging(df[,var_idx],i)
      St$lag_y <- lag_y
      name_id <- which(names(St) == "lag_y")
      names(St)[name_id] <-  paste0("Lag_", var, "_", as.character(i))
    }
  } else {
    num_lags_y <- 0
  }
  
  ## lags of other data 
  
  df_other <-df
  df_other[, var_idx] <- NULL
  df_other$Date <- NULL
  names_other <- names(df_other)
  
  if (num_other_lags == 1) {
    
    df_lag <- data.frame( apply(df_other, 2, FUN= lagging_1) )
    names(df_lag) <- unlist(lapply(names_other, FUN  = add_1))
    St <- cbind(St, df_lag)
    
  } else if (num_other_lags == 2) {
    
    df_lag <-  data.frame( apply(df_other, 2, FUN= lagging_1) )
    names(df_lag) <- unlist(lapply(names_other, FUN  = add_1))
    St <- cbind(St, df_lag)
    
    df_lag <-  data.frame( apply(df_other, 2, FUN= lagging_2) )
    names(df_lag) <- unlist(lapply(names_other, FUN  = add_2))
    St <- cbind(St, df_lag)
  }
  
  
  ## cross sectional factors 
  
  if (include_fac == T) {
    fac_names <- c()
    for (i in 1:num_fac) {
      fac_names <- c(fac_names, paste0(as.character(i), "_fac"))
    }
    
    
    pc <- prcomp(df_other, center = TRUE, scale. = TRUE)
    pcs <- pc$x[,1:num_fac]
    
    if (num_lag_fac == 1) {
      
      pc_lag1 <- data.frame(  apply(pcs, 2, FUN= lagging_1) )
      names(pc_lag1) <- unlist(lapply(fac_names, FUN  = add_1))
      St <- cbind(St, pc_lag1)
      
    } else if (num_lag_fac == 2) {
      
      pc_lag1 <-  data.frame( apply(pcs, 2, FUN= lagging_1) )
      names(pc_lag1) <- unlist(lapply(fac_names, FUN  = add_1))
      St <- cbind(St, pc_lag1)
      
      pc_lag2 <- data.frame( apply(pcs, 2, FUN= lagging_2) )
      names(pc_lag2) <- unlist(lapply(fac_names, FUN  = add_2))
      St <- cbind(St, pc_lag2)
      
    } 
  } else {
    num_lag_fac <- 0
  }
  
  ## Moving average factors 
  if (include_maf == T){
    #  pc <- prcomp(df$EMP, center = TRUE, scale. = TRUE)
    #  pcs <- pc$x[,1:num_maf]
    
    a <- lagging(df[,var_idx],1)
    
    for(i in 2:num_maf_lags){
      a <- cbind(a, lagging(df[,var_idx],i))
    }
    
    pc <- prcomp(a[(num_maf_lags+1):nrow(a),], center = TRUE, scale. = TRUE)
    pcs <- pc$x[,1:num_maf]
    
    # adding Nans in the front to ensure same length and adding to St 
    for (i in 1:num_maf){ 
      
      pc_to_add <- c(rep(NaN, num_maf_lags), pcs[,i])
      St <- cbind(St, pc_to_add)
      names(St)[ncol(St)] <- paste0("MAF_PC_", as.character(i))
      
    }
  } else {
    num_maf_lags <- 0
  } 
  
  
  
  cols_to_remove <- max(max(num_lag_fac, num_maf_lags,num_lags_y), 1) # always one lag anyway :)
  
  print(paste0("Please remove the first ", as.character(cols_to_remove), " since these contain NaNs creaated by the lagging!"))
  
  return( list(cols_to_remove, St))
  
}



# same for forecastin horizon with h >= 2

engineer_st_higher_lags <- function(h, df, var, num_lags_y, num_other_lags, num_fac, num_lag_fac, num_maf, num_maf_lags,  include_own_lags, include_fac, include_maf) { 
  
  var_idx <- which(names(df) == var)
  St <- data.frame(df$Date, df[, var_idx])
  
  
  ## own lags 
  if (include_own_lags == T) {
    for (i in h:(num_lags_y+h-1)) {
      lag_y <- lagging(df[,var_idx],i)
      St$lag_y <- lag_y
      name_id <- which(names(St) == "lag_y")
      names(St)[name_id] <-  paste0("Lag_", var, "_", as.character(i))
    }
  } else {
    num_lags_y <- 0
  }
  ## lags of other data 
  
  df_other <-df
  df_other[, var_idx] <- NULL
  df_other$Date <- NULL
  names_other <- names(df_other)
  
  # defining some new functions 
  
  lagging_here_1 <- function(x){
    l <- lagging(x,h)
    return(l)
  }
  
  lagging_here_2 <- function(x){
    l <- lagging(x,h+1)
    return(l)
  }
  
  add_here_1 <- function(x) {
    x <- paste0(x, "_", as.character(h))
    return(x)
  }
  
  add_here_2 <- function(x) {
    x <- paste0(x, "_", as.character(h+1))
    return(x)
  }
  
  
  
  if (num_other_lags == 1) {
    
    df_lag <- data.frame( apply(df_other, 2, FUN= lagging_here_1) )
    names(df_lag) <- unlist(lapply(names_other, FUN  = add_here_1))
    St <- cbind(St, df_lag)
    
  } else if (num_other_lags == 2) {
    
    df_lag <-  data.frame( apply(df_other, 2, FUN= lagging_here_1) )
    names(df_lag) <- unlist(lapply(names_other, FUN  = add_here_1))
    St <- cbind(St, df_lag)
    
    df_lag <-  data.frame( apply(df_other, 2, FUN= lagging_here_2) )
    names(df_lag) <- unlist(lapply(names_other, FUN  = add_here_2))
    St <- cbind(St, df_lag)
  }
  
  
  ## cross sectional factors 
  
  if (include_fac == T) {
    fac_names <- c()
    for (i in 1:num_fac) {
      fac_names <- c(fac_names, paste0(as.character(i), "_fac"))
    }
    
    # gettign principal components
    pc <- prcomp(df_other, center = TRUE, scale. = TRUE)
    pcs <- pc$x[,1:num_fac]
    
    if (num_lag_fac == 1) {
      
      pc_lag1 <- data.frame(  apply(pcs, 2, FUN= lagging_here_1 ))
      names(pc_lag1) <- unlist(lapply(fac_names, FUN  = add_here_1))
      St <- cbind(St, pc_lag1)
      
    } else if (num_lag_fac == 2) {
      
      pc_lag1 <-  data.frame( apply(pcs, 2, FUN= lagging_here_1 ))
      names(pc_lag1) <- unlist(lapply(fac_names, FUN  = add_here_1))
      St <- cbind(St, pc_lag1)
      
      pc_lag2 <- data.frame( apply(pcs, 2, FUN= lagging_here_2) )
      names(pc_lag2) <- unlist(lapply(fac_names, FUN  = add_here_2 ))
      St <- cbind(St, pc_lag2)
      
    } 
  } else {
    num_lag_fac <- 0
  }
  
  ## Moving average factors 
  if (include_maf == T){
    #  pc <- prcomp(df$EMP, center = TRUE, scale. = TRUE)
    #  pcs <- pc$x[,1:num_maf]
    
    a <- lagging(df[,var_idx],h)

    for(i in (h+1):(num_maf_lags+h-1)){
      a <- cbind(a, lagging(df[,var_idx],i))
    }
    
    pc <- prcomp(a[(num_maf_lags+h):nrow(a),], center = TRUE, scale. = TRUE)
    pcs <- pc$x[,1:num_maf]
    
    # adding Nans in the front to ensure same length and adding to St 
    for (i in 1:(num_maf)){ 
      
      pc_to_add <- c(rep(NaN, num_maf_lags+h-1), pcs[,i])
      St <- cbind(St, pc_to_add)
      names(St)[ncol(St)] <- paste0("MAF_PC_", as.character(i))
      
    }
  } else {
    num_maf_lags <- 0
  } 
  
  cols_to_remove <- max(max(num_lag_fac+h-1, num_maf_lags+h-1,num_lags_y+h-1), h) # always two lags anyway :)
  
  print(paste0("Please remove the first ", as.character(cols_to_remove), " since these contain NaNs creaated by the lagging!"))
  
  return( list(cols_to_remove, St))
  
}


### Function to call engineer_st on var and save output as csv-s correspondingly

## importantly, only the "St", not the "Xt" is computed here. Xt can simply be computed by removing
## the additional variables from the corresponding St data set. 


create_different_St <- function(df, var, freq, max_h) {
  
  if (freq == "monthly") {
    potential_hs <- c(3,6,9,12)
    hs_here <- potential_hs[potential_hs <= max_h]
  } else if (freq == "quarterly") {
    potential_hs <- c(2,4)
    hs_here <- potential_hs[potential_hs <= max_h]
  }
  
  # original St
  A  <-  engineer_st(df = df, var = var, num_lags_y = num_lags_y , num_other_lags = num_other_lags , num_fac = num_fac, num_lag_fac = num_lag_fac, num_maf = num_maf, num_maf_lags = num_maf_lags,  include_own_lags = T, include_fac = T, include_maf = T)
  B <- A[2][[1]]
  if (A[1][[1]] > 0) {
    B <- B[(A[1][[1]]+1):nrow(B), ]
  }
  save_path <- paste0("St_Xt/", "St_", as.character(var), "_lags_", as.character(1), "_", as.character(freq), ".csv")
  write.csv(B, save_path, row.names = F)
  
  for (h in hs_here) {

  # higher lags 
    print(paste0("Including Lag Num of ", as.character(h), "!"))
    
  A  <-  engineer_st_higher_lags(h, df = df, var = var, num_lags_y = num_lags_y , num_other_lags = num_other_lags , num_fac = num_fac, num_lag_fac = num_lag_fac, num_maf = num_maf, num_maf_lags = num_maf_lags,  include_own_lags = T, include_fac = T, include_maf = T)
  B <- A[2][[1]]
  if (A[1][[1]] > 0) {
    B <- B[(A[1][[1]]+1):nrow(B), ]
  }
  save_path <- paste0("St_Xt/", "St_", as.character(var), "_lags_", as.character(h), "_", as.character(freq), ".csv")
  write.csv(B, save_path, row.names = F)
}
}

