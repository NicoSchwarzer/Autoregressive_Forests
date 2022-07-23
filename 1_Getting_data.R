##################
## Getting Data ##
##################


# This file is used to retrieve the stationary UK MD data (monthly) 
# as well as the raw UK MD Data and apply aggregations (fir the quarterly analysis) transformations on it
# Also, further quarterly data is read in and transformations are applied to them to ensure stationary

# For all pre-processing steps, a large Excel File is read in in which I have manually indicates which transformations
# need to be undergone.


setwd("C:/Users/Nico/Documents/Uni/4. Sem/Seminar/Code_data")

if (!require("alfred")) devtools::install_github("onnokleen/alfred")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("flipTime")) install.packages("flipTime")
if (!require("aTSA")) install.packages("aTSA")


#api_key <- '40f873f527e2586e09e633e96056d968'
#fred <- FredR(api_key)

library(alfred)
library(FredR)
library(tidyverse)
library(zoo)
library(flipTime)
library(lubridate)
library(readxl)
library(aTSA)

# Specific functions for obtaining and transforming data to be sourced
source("1b_functions_getting_data.R")



###############################################
## Reading in overall dataset (monthly Data) ##
###############################################

### 1) Already stationary data 

base_data_uk_stat <- read.csv("balanced_uk_md.csv")
head(base_data_uk_stat)

## Checking for NaNs

sum(is.na(base_data_uk_stat))
# 76 

# deleting final rows because of NaNs
base_data_uk_stat <- base_data_uk_stat[base_data_uk_stat$Date <= "2021-07-01",]

sum(is.na(base_data_uk_stat))
# 0 


# finally adding 1 Year Gov't Bond Yield

yield1_1_year <- read.csv("uk_1_year_yield.csv")[, c("ï..Date", "Price")]
yield1_1_year$date <- as.Date(rev(seq(as.Date("1997-01-01"), as.Date("2022-01-01"), by = "month" )))
yield1_1_year$ï..Date <- NULL
names(yield1_1_year) <- c("BGS_1yrs_yld", "date")

# checking stationary

plot <- plot_new_series(yield1_1_year, "BGS_1yrs_yld", "1 Year GVT Bond Yields (%)")
plot

adf_check <- double_adf_check(yield1_1_year$BGS_1yrs_yld, "BGS_1yrs_yld", 2, 1, 2, 1,1)
adf_check
# nice 

yield1_1_year_diff <- diff(yield1_1_year$BGS_1yrs_yld, 1, 1)
yield1_1_year_diff_df <- data.frame(yield1_1_year_diff, yield1_1_year$date[2:nrow(yield1_1_year)])
names(yield1_1_year_diff_df) <- c("BGS_1yrs_yld", "Date")


base_data_uk_stat$Date <- as.Date(base_data_uk_stat$Date)
yield1_1_year_diff_df$Date <- as.Date(yield1_1_year_diff_df$Date)

# merging to UK MD
base_data_uk_stat <- base_data_uk_stat %>%
  inner_join(yield1_1_year_diff_df, by = "Date") %>%
  mutate(SPREAD = BGS_10yrs_yld - BANK_RATE)   # creating "Spread"

base_data_uk_stat %>%
  summarize(max_date = max(Date), min_date = min(Date))

# 1998-01-01 to 2021-01-07
base_data_uk_stat$X <- NULL

write.csv(base_data_uk_stat, "base_data_uk_stat.csv", row.names = F)
# This is now the DF for the monthly analysis!



### 2) Raw data (raw needed to apply aggregations)


# reading in datasets 'in levels'  - stationarity transformations occur
# after aggregation to monthly

base_data_uk <- read.csv("raw_uk_md.csv", sep = ",")
base_data_uk$X <- NULL
base_data_uk$Date <- as.Date(base_data_uk$Date)

head(base_data_uk)


# Considering NaNs: Before the year 2000 - vast amount of missing data

base_data_uk %>%
  drop_na() %>%
  summarize(max_date = max(Date), min_date = min(Date))


## Dataset containing all monthly data 
base_data_uk <- drop_na(base_data_uk)


# finally adding 1 Year Gov't Bond Yield
yield1_1_year <- read.csv("uk_1_year_yield.csv")[, c("ï..Date", "Price")]
yield1_1_year$date <- as.Date(rev(seq(as.Date("1997-01-01"), as.Date("2022-01-01"), by = "month" )))
yield1_1_year$ï..Date <- NULL
names(yield1_1_year) <- c("BGS_1yrs_yld", "date")


# merging together and calculating spread
base_data_uk <- base_data_uk %>% 
  rename(date = Date) %>%
  mutate(date =  as.Date(date)) %>%
  mutate(SPREAD = BGS_10yrs_yld - BANK_RATE) %>%
  dplyr::inner_join(yield1_1_year, by = "date")


# saving to csv!
write.csv(base_data_uk, "base_data_uk_monthly.csv", row.names = F)



### Transformations to make series comparable ###

# In the overview excel sheet  I have assigned a value to each data series that indicates which transformation the 
# respective series needs to undergo!

overview_sheet <- readxl::read_xlsx("data_description.xlsx", sheet = "data_overview")

overview_sheet_monthly <- overview_sheet %>%
  filter(Frequency == "monthly")



### Monthly data to quarterly ###

# Since the analysis shall also be conducted on a monthly basis, the monthly data shall be transformed to quarterly data
base_data_uk_quarterly <- df_to_quarterly(base_data_uk)

write.csv(base_data_uk_quarterly, "base_data_uk_quarterly.csv")


### Making the series stationary
names(base_data_uk_quarterly)[1] <- "date"

base_data_uk_quarterly_stat <- to_stationary(base_data_uk_quarterly, 1, 1, "m")

write.csv(base_data_uk_quarterly_stat, "base_data_uk_quarterly_stat.csv")



####################################
## Getting further quarterly data ##
####################################

# Further quarterly series are easily obtained via API queries. 

## GDP (seasonally adjusted - millions - 2010 chained)

gdp <- 'CLVMNACSCAB1GQUK'
GDP <- alfred::get_fred_series(gdp)
GDP <- GDP %>% drop_na()
names(GDP) <- c("date", "gdp")

## Gross Capital Formation 
inv <- 'GBRGFCFQDSMEI'
INV <- alfred::get_fred_series(inv)
INV <- INV %>% drop_na()
names(INV) <- c("date", "inv")


## Government Consumption Expenditure 
gvt_con <- 'GBRGFCEQDSMEI'
GVT_CON <- alfred::get_fred_series(gvt_con)
GVT_CON <- GVT_CON %>% drop_na()
names(GVT_CON) <- c("date", "gvt_con")

## Private Consumption Expenditure 
pr_con <- 'GBRPFCEQDSNAQ'
PR_CON <- alfred::get_fred_series(pr_con)
PR_CON <- PR_CON %>% drop_na()
names(PR_CON) <- c("date", "pr_con")


## Dwellings and residential permits issued for construction 
dw_con <- 'ODCNPI03GBQ657S'
DW_CON <- alfred::get_fred_series(dw_con)
DW_CON <- DW_CON %>% drop_na()
names(DW_CON) <- c("date", "dw_con")


##  Total Dwellings and Residential Buildings by Stage of Construction, Started
dw_start <- 'WSCNDW01GBQ470S'
DW_START <- alfred::get_fred_series(dw_start)
DW_START <- DW_START %>% drop_na()
names(DW_START) <- c("date", "dw_start")



## Real Changes in Invesntories 
ch_inv <- 'NINVRSAXDCGBQ'
CH_INV <-  alfred::get_fred_series(ch_inv)
CH_INV <- CH_INV %>% drop_na()
names(CH_INV) <- c("date", "ch_inv")


## houshold assets 
hh_assets <- read.csv("uk_hh_assets.csv")
hh_assets$date <- as.Date(as.yearqtr(hh_assets$date, format = "%Y Q%q") ) # date transformation 
names(hh_assets) <- c("date", "hh_assets")


## houshold liabilities 
hh_liab <- read.csv("uk_hh_liabilities.csv")
hh_liab$date <- as.Date(as.yearqtr(hh_liab$date, format = "%Y Q%q") ) # date transformation 
names(hh_liab) <- c("date", "hh_liab")

## houshold debt to income
hh_debt_ti <- read.csv("uk_hh_debt_to_income.csv")
hh_debt_ti$date <- as.Date(as.yearqtr(hh_debt_ti$date, format = "%Y Q%q") ) # date transformation 
names(hh_debt_ti) <- c("date", "hh_debt_ti")


## gross governemnt debt 
gvt_debt<- read.csv("uk_gvt_debt.csv")
gvt_debt$date <- as.Date(as.yearqtr(gvt_debt$date, format = "%Y Q%q") ) # date transformation 
names(gvt_debt) <- c("date", "gvt_debt")

# change ingross Government debt
chg_gvt_debt<- read.csv("uk_chg_gvt_debt.csv")
chg_gvt_debt$date <- as.Date(as.yearqtr(chg_gvt_debt$date, format = "%Y Q%q") ) # date transformation 
names(chg_gvt_debt) <- c("date", "chg_gvt_debt")

# total government revenue
gvt_rev<- read.csv("uk_gvt_revenue.csv")
gvt_rev$date <- as.Date(as.yearqtr(gvt_rev$date, format = "%Y Q%q") ) # date transformation 
names(gvt_rev) <- c("date", "gvt_rev")
    

      
## merging all into one DF  

quarterly_data <- GDP %>%
  inner_join(INV, by = "date") %>%
  inner_join(GVT_CON, by = "date") %>%
  inner_join(PR_CON, by = "date") %>%
  inner_join(DW_CON, by = "date") %>%
  inner_join(DW_START, by = "date") %>%
  inner_join(CH_INV, by = "date") %>%
  inner_join(hh_assets, by = "date") %>%
  inner_join(hh_liab, by = "date") %>%
  inner_join(hh_debt_ti, by = "date") %>%
  inner_join(gvt_debt, by = "date") %>%
  inner_join(chg_gvt_debt, by = "date") %>%
  inner_join(gvt_rev, by = "date") %>%
  # also merging in the CPI_ALL column since this is needed in the transformation step 
  inner_join(base_data_uk_quarterly[, c("date", "CPI_ALL")],  by = "date" )



### Transformations to make series comparable


# Again: In the overview excel sheet  I have assigned a value to each data series that indicates which transformation the 
# respective series needs to undergo, like creating real prices or have all prices in Million Pounds 

overview_sheet_quarterly <- overview_sheet %>%
  filter(Frequency == "quarterly")

quarterly_data <- transform_series(quarterly_data, "q")
quarterly_data$CPI_ALL <- NULL


# saving intermediate results
write.csv(quarterly_data, "further_quarterly_data.csv")


### Making the the further data stationary ### 


## 1) Run ADF Checks with (possible) transformation types 

names(quarterly_data)

## For GDP

plot <- plot_new_series(quarterly_data[, c("date", "gdp")], "gdp", "GDP in Million Pounds")
plot

# case drift and trend - transformation 5 

adf_check <- double_adf_check(quarterly_data$gdp, "gdp", 3, 1, 5, 1, 1)
adf_check


## For INV

plot <- plot_new_series(quarterly_data[, c("date", "inv")], "inv", "INV  in Million Pounds")
plot

# case drift and trend - transformation 5 
adf_check <- double_adf_check(quarterly_data$inv, "inv", 3, 1, 5, 1, 1)
adf_check


## For GVT_CON

plot <- plot_new_series(quarterly_data[, c("date", "gvt_con")], "gvt_con", "GVT Consumption")
plot

# case drift no trend - transformation 5 
adf_check <- double_adf_check(quarterly_data$gvt_con, "gvt_con", 2, 1, 5, 1, 1)
adf_check


## For PR_CON

plot <- plot_new_series(quarterly_data[, c("date", "pr_con")], "pr_con", "Private Consumption (Million Pounds)")
plot

# case drift and trend - transformation 5

adf_check <- double_adf_check(quarterly_data$pr_con, "pr_con", 3, 1, 5, 1, 1)
adf_check



## for dw_con

plot <- plot_new_series(quarterly_data[, c("date", "dw_con")], "dw_con", "Permits for new Residential Buildings (Growth Rate)")
plot

# case stationary - transformation 1
adf_check <- double_adf_check(quarterly_data$dw_con, "dw_con", 1, 1, 1, 1, 1)
adf_check



## For dw_start

plot <- plot_new_series(quarterly_data[, c("date", "dw_start")], "dw_start", "Residential Building currently Constructed (Num. Buildings)")
plot

# case drift and trend - transformation 2
adf_check <- double_adf_check(quarterly_data$dw_start, "dw_start", 3, 1, 2, 1, 1)
adf_check

## for ch_inv
plot <- plot_new_series(quarterly_data[, c("date", "ch_inv")], "ch_inv", "Changes in Inventories (Million Pounds) ")
plot

# stationary - still transformation 4
adf_check <- double_adf_check(quarterly_data$ch_inv, "ch_inv", 1, 1, 1, 1, 1)
adf_check

## for hh_assets
plot <- plot_new_series(quarterly_data[, c("date", "hh_assets")], "hh_assets", "Housholds- financial assets (Million Pounds) ")
plot

# case drift and trend - transformation 5
adf_check <- double_adf_check(quarterly_data$hh_assets, "hh_assets", 3, 1, 5, 1, 1)
adf_check


## For hh_liab
plot <- plot_new_series(quarterly_data[, c("date", "hh_liab")], "hh_liab", "Housholds- Liablities (Million Pounds) ")
plot

# case drift and no trend - transformation 5
adf_check <- double_adf_check(quarterly_data$hh_liab, "hh_liab", 2, 1, 5, 1, 1)
adf_check


## For hh_debt_ti
plot <- plot_new_series(quarterly_data[, c("date", "hh_debt_ti")], "hh_debt_ti", "Housholds- Debt to Income")
plot

# case drift and no trend - transformation 2
adf_check <- double_adf_check(quarterly_data$hh_debt_ti, "hh_debt_ti", 2, 1, 2, 1, 1)
adf_check


## For gvt_debt
plot <- plot_new_series(quarterly_data[, c("date", "gvt_debt")], "gvt_debt", "Real GVT Debt (Million Pounds)")
plot

# case drift and trend - transformation 5
adf_check <- double_adf_check(quarterly_data$gvt_debt, "gvt_debt", 3, 1, 5, 2, 2)
adf_check

## For chg_gvt_debt
plot <- plot_new_series(quarterly_data[, c("date", "chg_gvt_debt")], "chg_gvt_debt", "Change in GVT Debt (Million Pounds)")
plot

# case drift and no trend - transformation 5
adf_check <- double_adf_check(quarterly_data$chg_gvt_debt, "chg_gvt_debt", 2, 1, 5, 1, 1)
adf_check


## For gvt_rev
plot <- plot_new_series(quarterly_data[, c("date", "gvt_rev")], "gvt_rev", "GVT Revenue (Million Pounds)")
plot

# case drift and no trend - transformation 5
adf_check <- double_adf_check(quarterly_data$gvt_rev, "gvt_rev", 2, 1, 5, 1, 1)
adf_check


## Now applying the reasoable transformations 
overview_sheet <- readxl::read_xlsx("data_description.xlsx", sheet = "data_overview")

overview_sheet_quarterly <- overview_sheet %>%
  filter(Frequency == "quarterly")

new_quarterly_data_stat <- to_stationary(quarterly_data, 1, 1, "q")[, 1:14]



### Finally merging the transformed & stationary quarterly DF of the stationary monthly aggregates of the UK MD Data ###

all_data_quarterly <- new_quarterly_data_stat %>%
  inner_join(base_data_uk_quarterly_stat, by = "date")

# checking for nans
print(paste0("There are ", as.character(sum(is.na.data.frame(all_data_quarterly))) , " numbers of NaN Values in the final dataset!"))
# 0 - nice!

# checking for time period
all_data_quarterly %>%
  summarize(max_date = max(date), min_date = min(date))

# Q4 2001 to Q3 2021!

write.csv(all_data_quarterly, "all_data_quarterly.csv", row.names = F)
# This shall be the base DF for the quarterly analysis !!



