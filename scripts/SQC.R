library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv")
collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")

pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)
collraw$Col_Date<-as.Date(collraw$Col_Date)

sqc1<-left_join(x = pigraw,y = collraw,by=c("BoarID"="BoarID"))

sqc2<-sqc1 %>% 
  filter(`Boar Status`=='WORKING') %>% 
  group_by(BoarID) %>% 
  summarize(n=n())

sqc3<-left_join(x = sqc1,y = sqc2,by=c("BoarID"="BoarID"))

sqc4<-sqc3 %>% 
  filter(n>=10)

sqc5<- sqc4 %>% 
  filter(`Collection Status`!='NC') %>% 
  arrange(Col_Date) %>% 
  group_by(BoarID) %>% 
  top_n(6,Col_Date)

sqc6<-sqc5 %>% 
  filter(`Collection Status`=='TR')

sqc7<-sqc5 %>% 
  filter(`Collection Status`=='US')

sqc8<-anti_join(x = sqc6,y = sqc7,by=c("BoarID"="BoarID","Col_Date"="Col_Date"))

sqc9<-sqc8 %>% 
  group_by(BoarID) %>% 
  summarize("Collections Trashed Consecutively" =n_distinct(Col_Date))

sqc10<-sqc9 %>% 
  filter(`Collections Trashed Consecutively`>=6)

sqc11<-left_join(x = sqc10,y = pigraw,by=c("BoarID"="BoarID"))

sqc12<-sqc11 %>% 
  arrange(`Boar Stud`)

sqc13<-sqc12[c(3,1,4,6)]

write_csv(x = sqc13,path = here::here("data","sqc.csv"),append = FALSE)

### Calculate semen quality cull errors ###


sqc14<-sqc1 %>% 
  filter(`Boar Status`%in%c('CULLED','DEAD'),
         Date_Studout<floor_date(x = today(),unit = "week", week_start = 7),
         Date_Studout>=floor_date(x = today(),unit= "week", week_start = 7)-91) %>% 
  group_by(BoarID) %>% 
  summarize(n=n())

sqc15<-left_join(x = sqc1,y = sqc14,by=c("BoarID"="BoarID"))

sqc16<-sqc15 %>% 
  filter(n>=10)

sqc17<- sqc16 %>% 
  filter(`Collection Status`!='NC') %>% 
  arrange(Col_Date) %>% 
  group_by(BoarID) %>% 
  top_n(6,Col_Date)

sqc18<-sqc17 %>% 
  filter(`Collection Status`=='TR')

sqc19<-sqc17 %>% 
  filter(`Collection Status`=='US')

sqc20<-anti_join(x = sqc18,y = sqc19,by=c("BoarID"="BoarID","Col_Date"="Col_Date"))

sqc21<-sqc20 %>% 
  group_by(BoarID) %>% 
  summarize("Collections Trashed Consecutively" =n_distinct(Col_Date))

sqc22<-sqc15 %>% 
  filter(n>=8)

sqc23<- sqc22 %>% 
  arrange(Col_Date) %>% 
  group_by(BoarID) %>% 
  top_n(8,Col_Date)

sqc24<-sqc23 %>% 
  filter(`Collection Status`=='NC')

sqc25<-sqc23 %>% 
  filter(`Collection Status`%in%c('US','TR'))

sqc26<-anti_join(x = sqc24,y = sqc25,by=c("BoarID"="BoarID","Col_Date"="Col_Date"))

sqc27<-sqc26 %>% 
  group_by(BoarID) %>% 
  summarize("Consecutive Failed Collections" =n_distinct(Col_Date))

write_csv(x = sqc21, file = here::here("data","semenculls.csv"), append = FALSE)
write_csv(x = sqc27, file = here::here("data","trainingculls.csv"),append = FALSE)
