## library
library(openxlsx)
library(dplyr)
library(ggplot2)


## file
covid = read.csv('covid-19.csv')
SK_AGE = read.xlsx('SK_AGE.xlsx')
colnames(SK_AGE)

## Covid 전처리
covid_past_past_view = function(covid, days){
  covid_past = covid
  for(i in 2:4){
    covid_past[1,i] = 0
    for(q in 2:(days+1)){
      covid_past[q,i] = sum(covid[1:(q-1),i])
    }
    
    for(j in (days+1):121){
      covid_past[j,i] = sum(covid[(j-days):(j-1),i])
    }
  }
  covid_past = covid_past[covid_past$Date != '02-29',]
  return(covid_past)
}

covid_past = covid_past_past_view(covid,3)

## 1. SK 데이터를 연령대 구분없이 계산

# 기본 전처리
SK_AGE_CITY = SK_AGE[,-c(3,5,6)]
SK_AGE_CITY = SK_AGE_CITY %>% group_by(STD_YM, STD_YMD, CITY) %>% summarise_each(funs = sum)
SK_AGE_CITY$STD_MD = substr(SK_AGE_CITY$STD_YMD,5,8)
SK_AGE_CITY = SK_AGE_CITY[,c(1,2,34,3:33)]
SK_AGE_CITY$Year = substr(SK_AGE_CITY$STD_YMD,1,4)
SK_AGE_CITY = SK_AGE_CITY[,c(1,2,3,35,4:34)]
SK_AGE_CITY_2019 = SK_AGE_CITY[SK_AGE_CITY$Year == 2019, ]
SK_AGE_CITY_2020 = SK_AGE_CITY[SK_AGE_CITY$Year == 2020, ]
SK_AGE_CITY_2020 = SK_AGE_CITY_2020[SK_AGE_CITY_2020$STD_MD != '0229', ]

# 연령대 구분없이 합치기.
SK_AGE_CITY_2019_merge = SK_AGE_CITY_2019
SK_AGE_CITY_2019_merge$Moving_People = 0
for(i in 1:length(SK_AGE_CITY_2019_merge$STD_YM)){
  SK_AGE_CITY_2019_merge$Moving_People[i] = sum(SK_AGE_CITY_2019_merge[i,6:35])
}
SK_AGE_CITY_2019_merge = SK_AGE_CITY_2019_merge[,c(1:5, 36)]

SK_AGE_CITY_2020_merge = SK_AGE_CITY_2020
SK_AGE_CITY_2020_merge$Moving_People = 0
for(i in 1:length(SK_AGE_CITY_2020_merge$STD_YM)){
  SK_AGE_CITY_2020_merge$Moving_People[i] = sum(SK_AGE_CITY_2020_merge[i,6:35])
}
SK_AGE_CITY_2020_merge = SK_AGE_CITY_2020_merge[,c(1:5, 36)]

SK_AGE_CITY_merge = rbind(SK_AGE_CITY_2019_merge, SK_AGE_CITY_2020_merge)

write.csv(SK_AGE_CITY_merge, 'SK_AGE_CITY_merge.csv', row.names = F)

# 연령대 구분없이 연도에 따른 차이 및 비율 관찰

SK_AGE_diff = SK_AGE_CITY_2020_merge[, c(3,5,6)]
SK_AGE_diff[,3] = SK_AGE_CITY_2019_merge[,6] - SK_AGE_CITY_2020_merge[,6]

SK_AGE_diff_percent = SK_AGE_CITY_2020_merge[, c(3,5,6)]
SK_AGE_diff_percent[,3] = (SK_AGE_CITY_2019_merge[,6] - SK_AGE_CITY_2020_merge[,6])/SK_AGE_CITY_2019_merge[,6]
colnames(SK_AGE_diff_percent)[3] = 'Moving_People_Percent'

SK_AGE_diff = merge(SK_AGE_diff, SK_AGE_diff_percent)

write.csv(SK_AGE_diff, 'SK_AGE_diff.csv', row.names = F)

## three day correlation 관찰
# 코로나의 영향을 관찰할 수 있는 2월 21일 이후를 관찰.
correlation_view = function(covid_past, SK_AGE_diff, covid, i){
  covid_past = covid_past_past_view(covid,i)
  # Total vs 도시별 percent
  print('Total vs Percent')
  print('Seoul')
  print(cor.test(covid_past$Total[21:120], SK_AGE_diff[SK_AGE_diff$CITY == '서울특별시',]$Moving_People_Percent[21:120]))
  print('Daegu')
  print(cor.test(covid_past$Total[21:120], SK_AGE_diff[SK_AGE_diff$CITY == '대구광역시',]$Moving_People_Percent[21:120]))
  
  # 각 도시 vs 도시별 percent
  print('Each city vs Percent')
  print('Seoul')
  print(cor.test(covid_past$Seoul[21:120], SK_AGE_diff[SK_AGE_diff$CITY == '서울특별시',]$Moving_People_Percent[21:120]))
  print('Daegu')
  print(cor.test(covid_past$Daegu[21:120], SK_AGE_diff[SK_AGE_diff$CITY == '대구광역시',]$Moving_People_Percent[21:120]))
}

# 1일 관점
correlation_view(covid_past, SK_AGE_diff, covid, 1)
correlation_view(covid_past, SK_AGE_diff, covid, 3)
correlation_view(covid_past, SK_AGE_diff, covid, 5)
correlation_view(covid_past, SK_AGE_diff, covid, 7)


SK_AGE_diff = SK_AGE_CITY_2019[, c(3,5,6:35)]
for(i in 3:32){
  SK_AGE_diff[,i] = SK_AGE_CITY_2019[,(i+3)] - SK_AGE_CITY_2020[,(i+3)]
}

SK_AGE_diff_percent = SK_AGE_CITY_2019[, c(3,5,6:35)]
for(i in 3:32){
  SK_AGE_diff_percent[,i] = (SK_AGE_CITY_2019[,(i+3)] - SK_AGE_CITY_2020[,(i+3)])/SK_AGE_CITY_2019[,(i+3)]
}

write.csv(SK_AGE_diff, 'SK_AGE_diff.csv', row.names = F)
write.csv(SK_AGE_diff_percent, 'SK_AGE_diff_percent.csv', row.names = F)
