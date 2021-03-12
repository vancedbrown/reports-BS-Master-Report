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

ntc1<-left_join(x = pigraw,y = collraw,by=c("BoarID"="BoarID"))

ntc2<-ntc1 %>% 
  filter(`Boar Status`=='WORKING') %>% 
  group_by(BoarID) %>% 
  summarize(n=n())

ntc3<-left_join(x = ntc1,y = ntc2,by=c("BoarID"="BoarID"))

ntc4<-ntc3 %>% 
  filter(n>=8,
         Date_Arrival<=today()-56)

ntc5<- ntc4 %>% 
  arrange(Col_Date) %>% 
  group_by(BoarID) %>% 
  top_n(8,Col_Date)

ntc6<-ntc5 %>% 
  filter(`Collection Status`=='NC')

ntc7<-ntc5 %>% 
  filter(`Collection Status`%in%c('US','TR'))

ntc8<-anti_join(x = ntc6,y = ntc7,by=c("BoarID"="BoarID","Col_Date"="Col_Date"))

ntc9<-ntc8 %>% 
  group_by(BoarID) %>% 
  summarize("Consecutive Failed Collection Attempts" =n_distinct(Col_Date))

ntc10<-ntc9 %>% 
  filter(`Consecutive Failed Collection Attempts`>=8)

ntc11<-left_join(x = ntc10,y = pigraw,by=c("BoarID"="BoarID"))

ntc12<-ntc11 %>% 
  arrange(`Boar Stud`)

ntc13<-ntc12[c(3,1,4,6)]

write_csv(x = ntc13,path = here::here("data","ntc.csv"),append = FALSE)
