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
save_path <- paste0(path, "Plot/")


############################################# XML to Data Frame #################################################

# Service Key: Open API에서 데이터를 신청하면 발급해주는 Key
service_key <- "oRdTD4YImXu5iRcKADNTCPMKb4C21PCpIOTUHNkH%2Fi1mtkxB3iVjV%2BO3qIpegpKLs4NXykMxgYmOG3ltgsVhmQ%3D%3D"
start_date <- "20200101"

end_date <- ifelse(format(Sys.time(),'%H') < 18, 
                   gsub("-", "", today() - 1), 
                   gsub("-", "", today()))

# end_date <- "20210228"

# Regional from Open API
# 여러 Option을 조합해 데이터를 불러오는 형태. 아래 예시에서는 start date와 end date만 변수로 지정해줌.
xml_path <- paste0("http://openapi.data.go.kr/openapi/service/rest/Covid19/getCovid19SidoInfStateJson?serviceKey=",
                   service_key,"&pageNo=1&numOfRows=100000000&startCreateDay=",start_date,"&endCreateDt=",end_date,"&")

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
           DailyConfirmed = as.integer(as.character(localOccCnt)),
           DailyDeath = as.integer(as.character(deathCnt)) - c(0, as.integer(as.character(deathCnt)[-length(deathCnt)])),
           Country_Region = p) %>%
    select(Date, DailyConfirmed, DailyDeath, Country_Region)
  if(length(which(df_sub$DailyConfirmed < 0 | df_sub$DailyDeath < 0))){
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
           DailyConfirmed = as.integer(as.character(confirmed)) - c(0, as.integer(as.character(confirmed)[-length(confirmed)])),
           DailyDeath = as.integer(as.character(deceased)) - c(0, as.integer(as.character(deceased)[-length(deceased)])),
           Country_Region = p) %>%
    select(Date, DailyConfirmed, DailyDeath, Country_Region)
  if(length(which(df_sub$DailyConfirmed < 0 | df_sub$DailyDeath < 0))){
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
    mutate(Cases = cumsum(DailyConfirmed),
           Deaths = cumsum(DailyDeath),
           Days_after_First_Cases = seq_along(Date)) %>%
    select(Date, Cases, Deaths, DailyConfirmed, DailyDeath, Country_Region, Days_after_First_Cases)
  if(length(which(df_sub$DailyConfirmed < 0 & df_sub$DailyDeath < 0))){
    print(p)
  }
  if(is.null(df_fin)){
    df_fin <- df_sub
  }else{
    df_fin <- rbind(df_fin, df_sub)
  }
}

df_fin %>% filter(Date == "2020-09-18")
df_fin %>% filter(Date == "2020-07-01")

unique(df_fin$Country_Region)


province_list <- list("Domestic" = unique(df_fin$Country_Region), 
                      "Capital" = c("Seoul", "Gyeonggi-do", "Incheon"), 
                      "NonCapital" = unique(df_fin$Country_Region)[!unique(df_fin$Country_Region) %in% c("Seoul", "Gyeonggi-do", "Incheon")], 
                      "Chungcheong" = c("Chungcheongnam-do", "Chungcheongbuk-do", "Daejeon", "Sejong"), 
                      "Honam" = c("Jeollabuk-do", "Jeollanam-do", "Gwangju"), 
                      "Gyeongbuk" = c("Chungcheongbuk-do", "Daegu"), 
                      "Gyeongnam" = c("Chungcheongnam-do", "Busan", "Ulsan"), 
                      "Gangwon" = c("Gangwon-do"), 
                      "Jeju" = c("Jeju"))


# (2) df_fin_ch: 전국 각 지역을 3개 권역으로 묶어서 정리한 데이터
df_fin_ch <- NULL
for(p in names(province_list)){
  df_sub <- df_fin %>% group_by(Date) %>% 
    filter(Country_Region %in% province_list[[p]]) %>%
    summarize(Cases = sum(Cases), Deaths = sum(Deaths), DailyConfirmed = sum(DailyConfirmed), DailyDeath = sum(DailyDeath)) %>%
    mutate(Date, Cases, Deaths, DailyConfirmed, DailyDeath, Country_Region = p,
           Days_after_First_Cases = as.integer(as.Date(Date) - min(as.Date(Date)) + 1))
  
  if(is.null(df_fin_ch)){
    df_fin_ch <- df_sub
  }else{
    df_fin_ch <- rbind(df_fin_ch, df_sub)
  }
}

df_fin_ch$Date <- as.character(df_fin_ch$Date)

df_fin_final <- reshape(as.data.frame(df_fin_ch), timevar="Country_Region", idvar=c("Date", "Days_after_First_Cases"), direction="wide", sep = "_")


df_fin %>% filter(Date == "2020-09-18")
df_fin_ch %>% filter(Date == "2020-09-18")
df_fin_final %>% filter(Date == "2020-09-18")

# write.csv(df_fin_ch, paste0(data_path, "COVID_19_Cases_Korea_Merged_Fin_", end_date, ".csv"), row.names = F, quote = F)
# write.csv(df_fin, paste0(data_path, "COVID_19_Cases_Korea_Merged_", end_date, ".csv"), row.names = F, quote = F)
write.csv(df_fin_ch, paste0(data_path, "COVID_19_Cases_Korea_by_region_", end_date, ".csv"), row.names = F, quote = F)
write.csv(df_fin_final, paste0(data_path, "COVID_19_Cases_Korea_", end_date, ".csv"), row.names = F, quote = F)



library(stringr)
library(reshape2)


temp <- melt(data = df_fin_ch %>% filter(Country_Region == "Domestic") %>% select(DailyConfirmed, DailyDeath, Date), id.vars = c("Date"), measure.vars = c("DailyConfirmed", "DailyDeath"))
colnames(temp) <- c("Date", "Type", "Count")
temp$Date <- as.Date(temp$Date)

p <- ggplot(data = temp, aes(x = Date, y = Count, col = Type)) +
  geom_line() +
  scale_x_date(breaks = c(paste0("2020-",str_pad(1:12, 2, pad = "0"),"-01"), paste0("2021-",str_pad(1:12, 2, pad = "0"),"-01")) %>% as.Date) +
  labs(title = "COVID-19 Daily Cases",
       subtitle = paste0("Korea, ", as.character(min(as.Date(temp$Date))), "~", as.character(max(as.Date(temp$Date)))), x = "Date", y = "Cases")+
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(
    plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=2),
    plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="maroon", vjust=2),
    axis.text=element_text(size=14),
    axis.title=element_text(size=16),
    axis.text.x = element_text(angle = 70, hjust = 0.5, vjust=0.5, size = 12),
    legend.justification=c(1,0))

p

ggsave(p, filename = paste0(as.character(today()), "_Korea_total_Daily_Death_Case.png"),path = "~/covid_19/Korea_Prep/Plot/", 
       width = 12, height = 8, dpi = 450)


names(province_list)[1:3]
df_fin_ch$Date <- as.Date(df_fin_ch$Date)

p <- ggplot(data = df_fin_ch[which(df_fin_ch$Country_Region %in% names(province_list)[1:3]), ], 
            aes(x = Date, y = DailyConfirmed, col = Country_Region)) +
  geom_line() +
  scale_x_date(breaks = c(paste0("2020-",str_pad(1:12, 2, pad = "0"),"-01"), paste0("2021-",str_pad(1:12, 2, pad = "0"),"-01")) %>% as.Date) +
  labs(title = "COVID-19 Daily Cases", 
       subtitle = paste0("Korea, ", as.character(min(as.Date(temp$Date))), "~", as.character(max(as.Date(temp$Date)))), 
       x = "Date", y = "Confirmed Cases") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme(
    plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=2),
    plot.subtitle=element_text(size=16, hjust=0.5, face="italic", color="maroon", vjust=2),
    axis.text=element_text(size=14),
    axis.title=element_text(size=16),
    axis.text.x = element_text(angle = 70, hjust = 0.5, vjust=0.5, size = 12),
    legend.justification=c(1,0))

p

ggsave(p, filename = paste0(as.character(end_date), "_Korea_Confirmed_Cases_by_region.png"),path = "~/covid_19/Korea_Prep/Plot/", 
       width = 12, height = 8, dpi = 450)
