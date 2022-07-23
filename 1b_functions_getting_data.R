################################
## Functions for getting data ##
################################



library(lubridate)
library(tidyverse)
library(aTSA)


## This funtion maps the monthly data to quarterly data 
## Imprtantly, the aggregation function depends on the data
## Hence, the data overview table defines a specific operation which is applied here

df_to_quarterly <- function(input) {
  
  names_input <- names(input)
  quarterly_mat <- matrix(0, 82, ncol(base_data_uk))
  
  date_first <- as.character(input[,1][1])
  year_first <- as.integer(substr(date_first, 1, 4))
  month_first <-as.integer(substr(date_first, 6, 7))
  
  for (var in names(input)[2:length(input)]) {
    
    var_idx <- which(names(input) == var)
    aggr_value <- overview_sheet_monthly$Aggregate_to_q[overview_sheet_monthly$Variable_Name == var]
    
    monthly <- ts(input[,c(var)], start = c(year_first,month_first), frequency = 12)
    if (aggr_value == "mean") {   
      quarterly <- aggregate(monthly, nfrequency = 4, FUN = mean )
    } else if (aggr_value == "sum") {
      quarterly <- aggregate(monthly, nfrequency = 4, FUN = sum)
    }
    quarterly_mat[, var_idx] <- as.matrix(quarterly)
  }
  
  
  quarterly_df <- data.frame(quarterly_mat)
  # adding date column in 
  quarterly_df[,1] <- as.Date(seq(as.Date("2001-07-01"), as.Date("2021-12-01"), by = "quarter" ))
  names(quarterly_df) <- names_input
  
  return(quarterly_df)
  
}



### Function to make data stationary depending on the assigned code 
### in the data explanation excel sheet
### For data were differencing is needed, specify lag and difference!

to_stationary <- function(input, lag, difference, freq) {
  
  names_input <- names(input)
  quarterly_mat <- matrix(0, nrow(input), ncol(input))
  
  for (var in names(input)[2:length(names_input)]) {
    var_idx <- which(names(input) == var)

    if (freq == "m"){
    stat_value <- overview_sheet_monthly$Code[overview_sheet_monthly$Variable_Name == var]
    } else if (freq == "q") {
    stat_value <- overview_sheet_quarterly$Code[overview_sheet_quarterly$Variable_Name == var]
    }
    
    if (stat_value == 1) {
      quarterly_mat[, var_idx] <- as.matrix(input[, var_idx])
    } else if (stat_value == 2) {
      quarterly_mat[, var_idx] <- c(NaN, diff(input[, var_idx], lag, difference) ) # adding Nan in front to ensure equal length
    } else if (stat_value == 4) {
      quarterly_mat[, var_idx] <-  log(input[, var_idx])  
    } else if (stat_value == 5) {
      quarterly_mat[, var_idx] <- c(NaN, diff(log(input[, var_idx]), lag, difference))  # adding Nan in front to ensure equal length
    }
    
  }
  quarterly_stat_df <- data.frame(quarterly_mat)
  
  # adding date column in 
  quarterly_stat_df[,1] <- as.Date(seq(input$date[1], input$date[nrow(input)], by = "quarter" ))
  # removing first row 
  quarterly_stat_df <- quarterly_stat_df[2:nrow(quarterly_stat_df), ]
  
  names(quarterly_stat_df) <- names_input
  
  return(quarterly_stat_df)
  
}



### This function plots the new time series to visualize
### which ADF test might be necessary

plot_new_series <- function(data, y, name){
  
  name_2 <- as.character(y)
  idx <- which(names(data) == y)
  
  plot <- ggplot(data= data , aes(x=date,  y= data[,idx]) ) +
    geom_line(size = 0.03, color = "blue") + 
    xlab("Time") +
    ylab(as.character(name)) +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          axis.title=element_text(size=14,face="bold")  ,
          axis.text=element_text(size= 13, face="bold"),
          plot.subtitle = element_text(size = 15, hjust = 0.5),
          legend.text = element_text(size=14),
          plot.background = element_rect(fill="#F7FBFF"),
          legend.key =  element_rect(fill = "#F7FBFF") )
  
  wd <- getwd()
  
  name_save <- paste0(wd,"/plots_ADF/plot_", name_2, ".png")
  ggsave(name_save)
  
  return(plot)
}


### This function performs an ADF test (depending on the type) before 
### and after a chosen stationarity transformation 

### ADF Type: 1 - no drift, no trend, 2 - drfit, no trend, 3 - drift & trend
### Change Type: 1 - no change, 2 - first diff, 4 - Log, 5 - 2 & 4
### Nlag and Ndiff refer to the process of differencing to make series stationary

## dont confuse that with the maximal lag order in the ADF test, which is 10


double_adf_check <- function(data, name_save, adf_type_before, adf_type_after, change_type, nlag, ndiff ) {
  
  
  adf_before_1 <-  aTSA::adf.test(data, nlag = 10, output= F)
  
  ## ADF 
  if (adf_type_before == 1){  # on drfit, no trend
    adf_before <- adf_before_1$type1
  } else if (adf_type_before == 2){  # drfit, no trend
    adf_before <- adf_before_1$type2
  } else if (adf_type_before == 3){  # drfit, trend
    adf_before <- adf_before_1$type3
  }
  
  ## Transforming 
  if (change_type == 1) {
    lag <- c(0:4)
    ADF <-rep("-", 5)
    P_value <-rep("-", 5)
    adf_after <- data.frame(lag, ADF, P_value)
  } else if (change_type == 2) {
    data_new <- diff(data, nlag, ndiff)
    adf_after_1 <- aTSA::adf.test(data_new, nlag = 10, output= F)
  }  else if (change_type == 4) {
    data_new <- log(data)
    adf_after_1 <- aTSA::adf.test(data_new, nlag = 10, output= F)
  }  else if (change_type == 5) {
    data_new <- log(data)
    data_new <- diff(data_new, nlag, ndiff)
    adf_after_1 <- aTSA::adf.test(data_new, nlag = 10, output= F)
  }
  
  if (change_type != 1) {
    if (adf_type_after == 1){  # on drfit, no trend
      adf_after <- adf_after_1$type1
    } else if (adf_type_after == 2){  # drfit, no trend
      adf_after <- adf_after_1$type2
    } else if (adf_type_after == 3){  # drfit, trend
      adf_after <- adf_after_1$type3
    }
  }  
  
  
  # to one DF
  adf_df <- dplyr::inner_join(data.frame(adf_before), data.frame(adf_after), by = "lag")
  names(adf_df) <- c("Lag", "ADF", "P-value", "ADF", "P-value")
  if (change_type == 1) {
  adf_df[, 2:3] <- round(adf_df[, 2:3], 2)
  }
  else {
  adf_df <- round(adf_df, 2)
  }
  adf_df <- adf_df[2:nrow(adf_df), ]
  
  write.csv(adf_df, paste0(getwd(), "/tables_ADF/", name_save, ".csv"), row.names = F)
  
  return(adf_df)
  
}








## This function performs the transformations to ensure that th columns of the quarterly data are comparable column are measured at one level (e.g. all spending and monetary figures in million pounds)
## and that all nominal series are transformed to real series. In the overview excel sheet  I have assigned a value 
##to each data series that indicates which transformation the respective series needs to undergo!

transform_series <- function(data, freq) {

  ## Freq -> set to "m" if input data is monthly, else "q"
  if (freq == "m") {
    overview_sheet <- overview_sheet_monthly
    ignore_vars <- c("date")
  } else if (freq == "q") {
    overview_sheet <- overview_sheet_quarterly
    ignore_vars <- c("date", "CPI_ALL")
  }
  
  names_series <- names(data)
  
  for (var in names_series){
    
    var_index = which(names_series == var)
    
    
    if (!var %in% ignore_vars) {

      overview_sheet$`Pre-transformation`
      
      transformation_value <- overview_sheet$`Pre-transformation`[overview_sheet$Variable_Name == var]
      
      if (transformation_value == 1) {
        series <- data[, var_index] / 1000000
        data[, var_index] <- series
      } else if (transformation_value == 2) {
        series <- data[, var_index] / (data$CPI_ALL/100)
        data[, var_index] <- series
      } else if (transformation_value == 3) {
        series <- data[, var_index] / 1000000 / (data$CPI_ALL/100)
        data[, var_index] <- series
        
      }
    }
  }
  names(data) <- names_series
  return(data)
}






### This function turns daily data into monthly data ###


daily_to_monthly <- function(input) {
  
  
  #round dates down to week
  input$date <- floor_date(input$date, "month")
  input <- input %>%
    group_by(date) %>%
    summarize(mean = mean(input[,2]))
  
  input$date <- as.character(input$date)
  
  return(data.frame(input))
  
}


into_quarterly <- function(input, name_new) {
  
  ### This function turns monthly into quarterly data. 
  ###The name_new param is the name of the series in the output DF
  ###ALWAYS pass in the df with the date column starting in the beginning of a year!
  
  date_first <- as.character(input[,1][1])
  year_first <- as.integer(substr(date_first, 1, 4))
  month_first <-as.integer(substr(date_first, 6, 7))
  
  
  monthly <- ts(input[,2], start = c(year_first,month_first), frequency = 12)
  quarterly <- aggregate(monthly, nfrequency = 4, FUN = mean )
  a <- data.frame(Y=as.matrix(quarterly), date=time(quarterly))
  
  
  dates_vec <- c()
  
  for (i in a$date) {
    i <- as.character(i)
  #print(i)
  #print(nchar(i))
      if (nchar(i) == 4) {
      ##| (tail(strsplit(i, "")[[1]], n = 2)[1] == "0")) {
      i_new <- paste0(i, "-01-01")
    } else if  (tail(strsplit(i, "")[[1]], n = 2)[1] == "2")  {
      i_new <- paste0(substr(i,1,4), "-04-01")
    } else if  ( (tail(strsplit(i, "")[[1]], n = 2)[1] == "5") | (tail(strsplit(i, "")[[1]], n = 2)[1] == "."))  {
      i_new <- paste0(substr(i,1,4), "-07-01")
    } else if  (tail(strsplit(i, "")[[1]], n = 2)[1] == "7")  {
      i_new <- paste0(substr(i,1,4), "-10-01")
    }
    dates_vec <- c(dates_vec, i_new)
  }
  
  #dates_vec <- as.Date(dates_vec)
  
  df <- data.frame(as.Date(dates_vec), a$Y)
  names(df) <- c("date", name_new)
  
  return(df)
  
}


