# rm(list = ls()); gc()

setwd("/home/gjhuh/covid_19/data/")


library(dplyr)
library(tibble)
# install.packages('zoo')
library(zoo)
library(stringr)
# install.packages('sys')
library(sys)
# load data
setwd("/home/gjhuh/covid_19/data/")

# setwd("~/COVID-19/0902_data_preprocessing/")

today = Sys.Date()
today = gsub("-", "",today)
today = substr(today ,3,8)
# today = '200907'
orig_f = paste("OxCGRT_latest_",today,".csv" , sep = '')
url = "https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv"

download.file(url = url, destfile = orig_f , mode='wb',method = 'wget' )

orig_df <- read.csv(orig_f, header = FALSE, stringsAsFactors = FALSE)
tail(orig_df)
head(orig_df)

# setting column names
colnames(orig_df) <- orig_df[1, ]
orig_df <- orig_df[2:nrow(orig_df), ]
colnames(orig_df) <- gsub(" ","_",colnames(orig_df))
colnames(orig_df) <- gsub("/","_",colnames(orig_df))

# subset countries which are used
orig_df <- orig_df
orig_df$Date <- strptime(as.character(orig_df$Date), "%Y%m%d")


non_flag_col <- c(colnames(orig_df)[grepl(pattern = "^[CEH]{1}[[:digit:]]{1}", colnames(orig_df)) & !grepl("Flag", colnames(orig_df))], "ConfirmedCases")


data <- c()
for (each_country in unique(orig_df$CountryCode)) {
  # subset each country data
  temp_data <- orig_df[orig_df$CountryCode == each_country, ]
  
  # subset only national data
  temp_data <- temp_data[which(temp_data$Jurisdiction == "NAT_TOTAL"), ]
  temp_data[, "ConfirmedCases"] <- as.numeric(temp_data[, "ConfirmedCases"])
  
  # setting initial na values to 0
  temp_data[1, non_flag_col][which(is.na(temp_data[1, non_flag_col]) | temp_data[1, non_flag_col] == "")] = 0
  temp_data[, non_flag_col] = apply(temp_data[,non_flag_col], 2, function(x) x = na.locf(as.numeric(x)))
  temp_data[, "ConfirmedCases"] = na.locf(temp_data[, "ConfirmedCases"])
  
  # if cumulative sum is less than previous day, replace value with average previous and following data
  if (sum(diff(temp_data[, "ConfirmedCases"]) < 0) > 0) {
    odd_idx <- which((diff(temp_data[, "ConfirmedCases"]) < 0) %in% TRUE) + 1
    temp_data[odd_idx, "ConfirmedCases"] <- as.integer((temp_data[odd_idx - 1, "ConfirmedCases"] + temp_data[odd_idx + 1, "ConfirmedCases"]) / 2)
  }
  
  temp_data$DailyConfirmed =  c(0, diff(temp_data[, "ConfirmedCases"]))
  
  # subset with first day data
  first_day <- match(TRUE, temp_data$ConfirmedCases > 0)
  
  # is no data
  if (is.na(first_day)) {
    next
  }
  
  temp_data <- temp_data[first_day:nrow(temp_data),]
  temp_data$Days = 1:nrow(temp_data)
  data <- rbind(data, temp_data)
}

# (200720) Negative DailyConfirmed(ECU, FRA, LTU, SMR, UGA)
unique(data[which(data$DailyConfirmed < 0), "CountryCode"])
data[which(data$DailyConfirmed < 0), "DailyConfirmed"] = 0

# additive policy flag measurement
data_add <- data

# multiplicative policy flag measurement
data_mult <- data

# formatting each variables
for (i in 1:ncol(data)) {
  if (colnames(data)[i] %in% policy_summary$PolicyName) {
    each_policy <- colnames(data)[i]
    # when policy is ordinal with flag
    if (policy_summary[each_policy, "Type"] == "Ordinal" & policy_summary[each_policy, "hasFlag"] == TRUE) {
      max_level <- policy_summary[each_policy,"max"]
      nona <- !(is.na(data[,i]))
      
      # (curr_level + flag(if given)) / (max_level + flag(if given))
      data_add[nona, i] <- 
        apply(cbind(as.numeric(data_add[nona, i]), as.numeric(data_add[nona, i + 1])), 1, function(x) sum(x[1], x[2], na.rm = T)) / 
        (max_level + !(is.na(as.numeric(data_add[nona, i + 1]))))
      
      # (curr_level / max_level) * (1 + flag(if given))
      data_mult[nona, i] <- 
        apply(cbind((as.numeric(data_mult[nona, i]) / max_level), as.numeric(data_mult[nona, i + 1])), 1, function(x) prod(x[1], (1 + x[2]), na.rm = T))
    }
    
    # when policy is ordinal without flag
    else if (policy_summary[each_policy, "Type"] == "Ordinal" & policy_summary[each_policy, "hasFlag"] == FALSE) {
      max_level <- policy_summary[each_policy,"max"]
      nona <- !(is.na(data[,i]))
      data_add[nona, i] <- 
        (as.numeric(data_add[nona, i])) / (max_level)
      data_mult[nona, i] <- 
        (as.numeric(data_mult[nona, i])) / (max_level)
    }
    
    # when policy is numerical
    else if (policy_summary[each_policy,"Type"] == "USD") {
      nona <- !(is.na(data[,i]))
      data_add[,i] <- as.numeric(data_add[,i])
      data_add[!(is.na(data_add[,i])) & data_add[,i] > 0, i] <- log10(data_add[!(is.na(data_add[,i])) & data_add[,i] > 0, i])
      data_mult[,i] <- as.numeric(data_mult[,i])
      data_mult[!(is.na(data_mult[,i])) & data_mult[,i] > 0, i] <- log10(data_mult[!(is.na(data_mult[,i])) & data_mult[,i] > 0, i])
    }
  }
}

# Drop unused level
data_add$CountryCode = factor(data_add$CountryCode) 
data_mult$CountryCode = factor(data_mult$CountryCode)

# Drop Flag Columns
data_add = data_add[,!str_detect(colnames(data_add), "Flag")] 
data_mult = data_mult[,!str_detect(colnames(data_mult), "Flag")]

data_add[, colnames(data_add) %in% policy_summary$PolicyName] = data_add[, colnames(data_add) %in% policy_summary$PolicyName] * 100
# data_mult[, colnames(data_mult) %in% policy_summary$PolicyName] = data_mult[, colnames(data_mult) %in% policy_summary$PolicyName] * 100
output_add = paste("data_additive_measure_", today, ".csv", sep='')
# output_mult = paste("data_multiplicative_measure_", today, ".csv", sep='')
write.csv(data_add, output_add, row.names = F)
# write.csv(data_mult, output_mult, row.names = F)