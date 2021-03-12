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
