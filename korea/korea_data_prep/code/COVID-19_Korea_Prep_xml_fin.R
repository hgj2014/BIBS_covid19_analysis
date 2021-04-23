rm(list = ls()); gc()

library("xml2")
library(tidyverse)
library(lubridate)
library(dplyr)

path <- "~/covid_19/Korea_Prep/"
# path <- "C:/test/COVID-19/Korea_Prep/"
ref_path <- paste0(path, "Ref/")
data_path <- paste0(path, "Data/")
code_path <- paste0(path, "Code/")
save_path <- paste0(path,"Plot/")

############################################# XML to Data Frame #################################################

# Service Key: Open API에서 데이터를 신청하면 발급해주는 Key
service_key <- "oRdTD4YImXu5iRcKADNTCPMKb4C21PCpIOTUHNkH%2Fi1mtkxB3iVjV%2BO3qIpegpKLs4NXykMxgYmOG3ltgsVhmQ%3D%3D"
# service_key <- "Y4khq%2FTeQ8ecYbWnp%2BBjndba%2B%2FAvdsx6OcLrnq9qtW0gOgWCzMdE0uOJFiblQwIujZHGBT8EWUECGJ9DADHkow%3D%3D"
start_date <- "20200101"
end_date <- gsub("-", "", today())
#end_date <- "20201120"
# end_date를 today로 설정하더라도 업데이트가 덜 된 경우 

# Regional from Open API
# 여러 Option을 조합해 데이터를 불러오는 형태. 아래 예시에서는 start date와 end date만 변수로 지정해줌.
xml_path <- paste0("http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19SidoInfStateJson?serviceKey=",service_key,"&pageNo=1&numOfRows=100000000&startCreateDay=",start_date,"&endCreateDt=",end_date,"&")

raw_xml <- read_xml(xml_path)
xml_node <- xml_children(raw_xml)
# xml_node_2: xml 파일의 body에 포함되는 element
xml_node_2 <- xml_children(xml_node)
# xml_items: xml_node_2의 세 번째 노드인 items이 우리가 원하는 확진자 정보를 나타내므로 해당 노드의 element를 call
xml_items <- xml_node_2[[3]] %>% xml_children()

# 각 xml_items가 한 날짜, 한 지역의 확진자 수를 포함하고 있으므로 해당 items
df <- lapply(seq_along(xml_items),
             function(i){
               temp_row <- xml_find_all(xml_items[i], "./*")
               tibble(
                 idx = i,
                 key = temp_row %>% xml_name(),
                 value = temp_row %>% xml_text()
               ) %>% return()
             }
) %>% bind_rows() %>%
  spread(key, value) %>%
  select(xml_items %>% xml_children() %>% xml_name() %>% unique())

table(df$localOccCnt)


filter(df, stdDay == "2020년 09월 20일 00시")

############################################# Open API Data Prep #################################################

# 영문 지역명이 없는 경우를 imputation
ref_gubun <- data.frame(KR = df$gubun, EN = df$gubunEn)
ref_gubun <- ref_gubun[which(ref_gubun$EN != ""),]
ref_gubun <- ref_gubun[!duplicated(ref_gubun),]
df$gubunEn[which(df$gubunEn == "")] <- as.character(ref_gubun$EN[match(df$gubun[which(df$gubunEn == "")], ref_gubun$KR)])

# 측정 시간: 0시 or 16시 
# 0시 기준이 아닌 측정값: 3월 2일 - NA 데이터
# 3월 1일 - NA 데이터
error_idx <- as.integer(which(sapply(df$stdDay, function(x) strsplit(x, split = " ")[[1]][4]) == "16시"))
df_error <- df[error_idx,]
df_ch_1 <- df[-error_idx,]

df_ch_test <- df_ch_1[df_ch_1$gubun == "검역",]
setdiff(unique(df_ch_1$stdDay),df_ch_test$stdDay)

df_ch_2 <- df_ch_1[df_ch_1$gubun != "검역" & df_ch_1$gubun != "합계",]
table(df_ch_2$gubun)
table(df_ch_2$stdDay)

df_ch_2$stdDay <- as.character(sapply(df_ch_2$stdDay, 
                                      function(x){
                                        temp <- unlist(strsplit(gsub("일", "",gsub("월", "",gsub("년", "", x))), split = " "))
                                        if(nchar(temp[2]) == 1){
                                          temp[2] <- paste0("0", temp[2])
                                        }
                                        if(nchar(temp[3]) == 1){
                                          temp[3] <- paste0("0", temp[3])
                                        }
                                        return(paste(temp[1:3], collapse = "-"))
                                      }))

region <- unique(as.character(df_ch_2$gubun))
date <- unique(as.character(df_ch_2$stdDay))

df_date <- sapply(seq_along(region), function(x){
  sapply(seq_along(date), function(y){
    #x <- 1
    #y <- 1
    return(nrow(df_ch_2[df_ch_2$gubun == region[x] & df_ch_2$stdDay == date[y],]))
  })
})

# 한 날짜에 여러 번 데이터가 생성된 경우 가장 마지막에 수정된 데이터만 가져옴

idx_rm_dup <- sapply(seq_along(region), function(x){
  sapply(seq_along(date), function(y){
    idx <- which(df_ch_2$gubun == region[x] & df_ch_2$stdDay == date[y])
    temp <- df_ch_2[idx,]
    if(length(idx) > 1){
      return(idx[which.max(c(ymd_hms(temp$createDt[1]), ymd_hms(temp$createDt[2])))])
    }
    return(idx)
  })
})

df_uniq <- df_ch_2[as.integer(idx_rm_dup),]
#2703/17

table(df_uniq$localOccCnt)

# Data from public data fortal - 3월 4일 ~
# https://data.go.kr/
df_1 <- data.frame(date = as.character(df_uniq$stdDay),
                   time = rep(0, nrow(df_uniq)),
                   province = as.character(df_uniq$gubunEn),
                   confirmed = df_uniq$localOccCnt,
                   deceased = df_uniq$deathCnt)

cum.na <- function(x) {
  x[which(is.na(x))] <- 0
  return(cumsum(x))
}

prov_list <- unique(as.character(df_uniq$gubunEn))
df_1 <-NULL
for(p in prov_list){
  df_sub <- filter(df_uniq, gubunEn == p) %>% arrange(stdDay) %>%
    mutate(Date = as.character(stdDay), 
           Case_Type = "Confirmed", 
           Difference = as.integer(as.character(localOccCnt)),
           Country_Region = p) %>%
    select(Date, Case_Type, Difference, Country_Region)
  if(length(which(df_sub$Difference < 0))){
    print(p)
  }
  if(is.null(df_1)){
    df_1  <- df_sub
  }else{
    df_1  <- rbind(df_1 , df_sub)
  }
}




# Data from Kaggle - 1월 20일 ~ 6월 30일
# https://www.kaggle.com/kimjihoo/coronavirusdataset/
df_kaggle <- read.csv(paste0(ref_path, "TimeProvince.csv")) # 3월 1일까지는 16시 기준, 3월 1일부터는 0시 기준 확진자
province <- as.character(df_kaggle$province)
province[which(province == "Jeju-do")] <- "Jeju"
df_kaggle$province <- province

df_2 <-NULL
for(p in prov_list){
  df_sub <- filter(df_kaggle, province == p) %>% arrange(date) %>%
    mutate(Date = as.character(date), 
           Case_Type = "Confirmed", 
           Difference = as.integer(as.character(confirmed)) - c(0, as.integer(as.character(confirmed)[-length(confirmed)])),
           Country_Region = p) %>%
    select(Date, Case_Type, Difference, Country_Region)
  if(length(which(df_sub$Difference < 0))){
    print(p)
  }
  if(is.null(df_2)){
    df_2  <- df_sub
  }else{
    df_2  <- rbind(df_2 , df_sub)
  }
}

############################################# Kaggle & Open API Data Merge #################################################

max(as.Date(df_1$Date))

df_2

# Kaggle 데이터 (1월 20일 ~ 6월 30일) + 공공데이터포털 (7월 1일 ~ 8월 17일)
df_1_sub <- filter(df_1, as.Date(Date) > max(as.Date(df_2$Date))) %>%
  arrange(Date)
df_total <- rbind(df_2[order(as.Date(df_2$Date)),], 
                  df_1_sub) %>%
  arrange(Date, Country_Region)


# (1) df_fin: 전국 각 지역별로 정리된 데이터
df_fin <- NULL
for(p in prov_list){
  df_sub <- filter(df_total, Country_Region == p) %>%
    mutate(Cases = cumsum(Difference),
           Days_after_First_Cases = seq_along(Date)) %>%
    select(Date, Case_Type, Cases, Difference, Country_Region, Days_after_First_Cases)
  if(length(which(df_sub$Difference < 0))){
    print(p)
  }
  if(is.null(df_fin)){
    df_fin <- df_sub
  }else{
    df_fin <- rbind(df_fin, df_sub)
  }
}

df_fin %>% filter(Date == "2020-09-18")


# (2) df_fin_ch: 전국 각 지역을 3개 권역으로 묶어서 정리한 데이터
df_fin_ch <- NULL
province <- c("전국(지역)", "수도권(서울+경기+인천)", "비수도권")
for(p in province){
  if(p == province[1]){
    df_sub <- df_fin %>% group_by(Date) %>%
      summarize(Cases = sum(Cases), Difference = sum(Difference)) %>%
      mutate(Date, Case_Type = "Confirmed",
             Cases, Difference, Country_Region = p,
             Days_after_First_Cases = as.integer(as.Date(Date) - min(as.Date(Date)) + 1))
  }else if(p == province[2]){
    df_sub <- df_fin %>% filter(Country_Region == "Seoul" | Country_Region == "Gyeonggi-do" | Country_Region == "Incheon") %>%
      group_by(Date) %>%
      summarize(Cases = sum(Cases), Difference = sum(Difference)) %>%
      mutate(Date, Case_Type = "Confirmed",
             Cases, Difference, Country_Region = p,
             Days_after_First_Cases = as.integer(as.Date(Date) - min(as.Date(Date)) + 1))
  }else if(p == province[3]){
    df_sub <- df_fin %>% filter(Country_Region != "Seoul" & Country_Region != "Gyeonggi-do" & Country_Region != "Incheon") %>%
      group_by(Date) %>%
      summarize(Cases = sum(Cases), Difference = sum(Difference)) %>%
      mutate(Date, Case_Type = "Confirmed",
             Cases, Difference, Country_Region = p,
             Days_after_First_Cases = as.integer(as.Date(Date) - min(as.Date(Date)) + 1))
  }
  if(is.null(df_fin_ch)){
    df_fin_ch <- df_sub
  }else{
    df_fin_ch <- rbind(df_fin_ch, df_sub)
  }
}
df_fin %>% filter(Date == "2020-09-18")
df_fin_ch %>% filter(Date == "2020-09-18")

file_date <- paste0(c("20",unlist(strsplit(as.character(max(as.Date(df_fin_ch$Date))), split = "-"))[2:3]), collapse = "")
write.csv(df_fin_ch, paste0(data_path, "COVID_19_Confirmed_Korea_Merged_Fin_", file_date, ".csv"), row.names = F, quote = F)
write.csv(df_fin, paste0(data_path, "COVID_19_Confirmed_Korea_Merged_", file_date, ".csv"), row.names = F, quote = F)


######################################### summary #########################################
# result data
# data_add -> additive measure policy data
# data_mult -> multiplicative measure policy data

library(dplyr)
library(tibble)
library(zoo)
library(stringr)
library(sys)


setwd(data_path)

orig_f = paste("OxCGRT_latest_",file_date,".csv" , sep = '')
url = "https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv"

#download.file(url = url, destfile = orig_f , mode='wb',method = 'wget' )

orig_df <- read.csv(url, header = FALSE, stringsAsFactors = FALSE)
if(!file.exists(paste0(data_path, orig_f))){
  orig_df_save <- orig_df
  names(orig_df_save) <- orig_df_save[1,]
  orig_df_save <- orig_df_save[-1,]
  write.csv(orig_df_save, paste0(data_path, orig_f), row.names = F)
  orig_df_save[1:5,1:5]
}


tail(orig_df)
# orig_df
country_list <- read.csv(paste0(ref_path,"CountryNameList_200710.csv"), stringsAsFactors = FALSE)
policy_summary <- read.csv(paste0(ref_path, "policySummary.csv"), stringsAsFactors = FALSE)

# setting column names
colnames(orig_df) <- orig_df[1, ]
colnames(orig_df) <- gsub(" ","_",colnames(orig_df))
colnames(orig_df) <- gsub("/","_",colnames(orig_df))
rownames(policy_summary) <- policy_summary$PolicyName

# adding country code to country list
before_country_list = country_list[,1]
country_list
country_list <- merge(country_list, unique(select(orig_df, CountryName, CountryCode)), by = "CountryName")

# (200818) Note. Countries not included in orig_df (8 Countries)
before_country_list[which(!before_country_list %in% country_list[,1])]

# subset countries which are used
df <- orig_df %>% filter(orig_df$CountryName %in% country_list$CountryName)
df$Date <- strptime(as.character(df$Date), "%Y%m%d")

# remove unnecessary columns
df <- df[, setdiff(colnames(df), c("CountryName", "ConfirmedDeaths","StringencyIndexForDisplay", "StringencyLegacyIndex", "StringencyLegacyIndexForDisplay", "GovernmentResponseIndex", "GovernmentResponseIndexForDisplay", "ContainmentHealthIndex", "ContainmentHealthIndexForDisplay", "EconomicSupportIndex", "EconomicSupportIndexForDisplay", "M1_Wildcard"))]
policy_summary$PolicyName = gsub("\\/", "_", policy_summary$PolicyName)
rownames(policy_summary) = gsub("\\/", "_", rownames(policy_summary))

non_flag_col <- names(df) %in% c(policy_summary[policy_summary$Type != "Flag", "PolicyName"], "ConfirmedCases")

# (200818) Note. ConfirmedCases of "TWN" are not reported.
df[which(df$CountryCode == "TWN"), "ConfirmedCases"]
df = df[which(df$CountryCode != "TWN"),]

data <- c()
for (each_country in unique(df$CountryCode)) {
  # subset each country data
  temp_data <- df[df$CountryCode == each_country, ]
  temp_data <- temp_data[temp_data$RegionName == "", ]
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

# Drop Flag Columns
data_add = data_add[,!str_detect(colnames(data_add), "Flag")] 

data_add[, colnames(data_add) %in% policy_summary$PolicyName] = data_add[, colnames(data_add) %in% policy_summary$PolicyName] * 100

output_add = paste("data_additive_measure_", file_date, ".csv", sep='')
write.csv(data_add, output_add)

# output_add
##################### korea data #####################

confirmed_f = paste0('COVID_19_Confirmed_Korea_Merged_Fin_',file_date, '.csv')
confirmed_data = read.csv(confirmed_f)

tail(confirmed_data)
data = data_add[which(data_add$CountryCode == "KOR"), !(colnames(data_add) %in% c("ConfirmedCases", "DailyConfirmed"))]
# data = data_mult[which(data_mult$CountryCode == "KOR"), !(colnames(data_mult) %in% c("ConfirmedCases", "DailyConfirmed"))]


length(data$Date)
length(data$Date)
confirmed_data$Date

confirmed_data$Date <- as.character(confirmed_data$Date)
confirmed_data$Country_Region <- gsub("전국(지역)", "Domestic", confirmed_data$Country_Region, fixed = T) %>%
  gsub("수도권(서울+경기+인천)", "Capital", ., fixed = T) %>% gsub("비수도권", "NonCapital", .)
names(confirmed_data)[match(c("Cases", "Difference"), names(confirmed_data))] <- c("ConfirmedCases", "DailyConfirmed")

data$Date <- as.character(data$Date)


df_conf <- sapply(unique(as.character(confirmed_data$Country_Region)), function(C_r){
  # C_r <- unique(as.character(confirmed_data$Country_Region))[1]
  if(C_r == unique(as.character(confirmed_data$Country_Region))[1]){
    df_res <- confirmed_data[confirmed_data$Country_Region == C_r, c("Date", "Days_after_First_Cases", "ConfirmedCases", "DailyConfirmed")]
  }else{
    df_res <- confirmed_data[confirmed_data$Country_Region == C_r, c("ConfirmedCases", "DailyConfirmed")]
  }
  return(df_res)
}, simplify = F)

df_conf_merge <- do.call(cbind, df_conf)
names(df_conf_merge)[1:2] <- c("Date", "Days")
names(df_conf_merge) <- gsub(".", "_", names(df_conf_merge), fixed = T)


df_merge <- merge(data, df_conf_merge, by = "Date", all = T)
df_merge$Date <- as.character(df_merge$Date)


df_merge$StringencyIndex[which(df_merge$StringencyIndex == "")] = NA
df_merge$StringencyIndex = na.locf(df_merge$StringencyIndex)


korea_add_f <- paste("KOR_additive_measure_",file_date,".csv", sep='')
korea_add_out <- paste("KOR_additive_measure_cluster_",file_date,".csv", sep='')

#write.csv(df_merge, korea_add_f, row.names = F)

########### input csv ###############
# calculate days and standardized Government index
gov_info = read.csv(paste0(ref_path, "KOR_Government_info.csv")) # raw_input
gov_info$Date <- as.Date(as.character(gov_info$Date), format='%Y%m%d')

df_merge_sub <- df_merge %>% filter(Date >= min(gov_info$Date))
df_merge_sub$Government <- 0

for(d in 1:length(gov_info$Date)){
  df_merge_sub$Government[df_merge_sub$Date>=gov_info$Date[d]] <- gov_info$Raw_Government[d]
}

df_merge_fin <- df_merge_sub %>% filter(Days >= 1) %>%
  select(c("CountryCode", "Jurisdiction", "Date", "C1_School_closing", "C2_Workplace_closing", "C3_Cancel_public_events", "C4_Restrictions_on_gatherings", "C5_Close_public_transport", "C6_Stay_at_home_requirements", "C7_Restrictions_on_internal_movement", "C8_International_travel_controls", "E1_Income_support", "E2_Debt_contract_relief", "H1_Public_information_campaigns", "H2_Testing_policy", "H3_Contact_tracing", "H6_Facial_Coverings", "StringencyIndex", "Days", "Domestic_ConfirmedCases", "Domestic_DailyConfirmed", "Capital_ConfirmedCases", "Capital_DailyConfirmed", "NonCapital_ConfirmedCases", "NonCapital_DailyConfirmed", "Government"))

write.csv(df_merge_fin, paste0(data_path, korea_add_f))


data_date <- "201231"

# COVID_19_Confirmed_Korea_Merged_fin: From Kaggle & Public Data Portal
df <- read.csv(paste0(data_path, "COVID_19_Confirmed_Korea_Merged_Fin_", data_date, ".csv"))


bp_list <- as.Date(c("2020-02-17", # 신천지 31번 확진
                     "2020-08-10", # 여름휴가 성수기 (~8월 초) 종료 후 첫 월요일
                     "2020-10-12"))# 사회적 거리두기 1단계로 격하 

library(stringr)

region_list <- as.list(unique(as.character(df$Country_Region)))
names(region_list) <- c("Domestic", "Capital", "Non-Capital")

df_ch <- df %>% mutate(Date = as.Date(Date)) %>% 
  mutate(Region = names(region_list)[match(Country_Region, region_list)])

p_2_1 <- ggplot(df_ch, aes(x = Date, y = Difference, col = Region)) +
  geom_line() +
  scale_x_date(breaks = paste0("2020-",str_pad(1:12, 2, pad = "0"),"-01") %>% as.Date) +
  labs(title = "COVID-19 Daily Cases", 
       subtitle = paste0("Korea, ", as.character(min(as.Date(df$Date))), "~", as.character(max(as.Date(df$Date)))), x = "Date", y = "Confirmed Cases")+
  geom_vline(xintercept = bp_list, color = "grey", size = 2, linetype="dashed")+
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme(
    plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=2),
    plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="maroon", vjust=2),
    axis.text=element_text(size=14),
    axis.title=element_text(size=16),
    axis.text.x = element_text(angle = 70, hjust = 0.5, vjust=0.5, size = 12),
    legend.justification=c(1,0))
p_2_1
ggsave(p_2_1, filename = paste0(as.character(data_date), "_Daily_case_in_Korea_by_Region_total.png"), path = save_path, width = 12, height = 7, dpi = 300)


# 
# data_list = c(korea_add_f)
# for( input_f in data_list){
#   data = read.csv(paste0(data_path, input_f))
#   #####################################
#   data$Government = 0
#   
#   latest_days = data$Days[length(data$Days)]
#   ref_days = c(gov_info$days,latest_days)
#   ref_days
#   
#   for(i in 1:(length(ref_days)-1)){
#     print(i)
#     data[ref_days[i+1]:ref_days[i], "Government"] = gov_info$Government[i]
#   }
#   # output_f = paste('KOR')
#   write.csv(data, paste0(data_path, input_f))
#   
# }

#system("conda activate tf_gpu")

# system(paste0("python3 ", code_path, 
#               "COVID-19_Korea_clustering.py ", data_path, 
#               " ", file_date,
#               " ", save_path)
# )
