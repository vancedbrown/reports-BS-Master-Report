library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(here)

source('C:/Users/vance/Documents/myR/functions/getSQL.r')
index <- "SELECT [Start_Date]
,[Interval]
,[StudID]
,[Breed]
,[Result]
FROM [Intranet].[dbo].[Boar_KPIs]
WHERE [KPI]='IndexPerDistributedDose'
AND [StudID] in (
'High Desert',
'MB 7081',
'MB 7082',
'MB 7092',
'MB 7093',
'MB 7094',
'MBW Cimarron',
'MBW Cyclone',
'MBW Illinois',
'MBW Yuma',
'Princeton',
'Skyline Boar Stud',
'SPGNC',
'SPGVA',
'SPGTX',
'Norson',
'GCM7-4')"
indexraw <- getSQL('Intranet',query=index)

indexraw$Start_Date<-as.Date(indexraw$Start_Date)

ind1<-indexraw %>% 
  filter(Start_Date>=floor_date(today(),unit = "week",week_start = 1)-364,
         Start_Date<floor_date(today(),unit = "week",week_start = 1),
         Interval=='WEEK',
         Breed==ifelse(StudID=='MBW Illinois','SPG120','SPG240'))

#write_csv(ind1,'index.csv')

ind1<-ind1[order(ind1$StudID,ind1$Start_Date),]

ind2<-ind1 %>%
  group_by(StudID) %>% 
  mutate(dr=row_number(Start_Date)-1)

ind3<-ind2 %>% 
  filter(dr==0) %>% 
  mutate(start=Result)

ind3<-ind3[c(3,7)]

ind4<-left_join(x = ind2,y = ind3,by=c("StudID"="StudID"))

ind5<-ind4 %>% 
  mutate(target=ifelse(StudID=='MBW Illinois',start+(dr*0.10),start+(dr*0.020)))

ind6<-ind5 %>%
  group_by(StudID) %>% 
  summarise(`Index Trend`=lm(Result~target)$coefficients[2])

write_csv(x = ind6,path = here::here("data","indextrend.csv"),append = FALSE)







