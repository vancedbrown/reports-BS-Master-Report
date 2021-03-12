library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

distraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/dist.csv")
splitraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/split.csv")

distraw$Date_Shipped<-as.Date(distraw$Date_Shipped)
splitraw$Collnum<-as.numeric(splitraw$Collnum)

batch1<-distraw %>% 
  filter(Dest=='* TRASH *')

batch2<-distraw %>% 
  filter(Dest!='* TRASH *')

batch3<-anti_join(x = batch2,y = batch1,by=c("Boar Stud"="Boar Stud","BatchNum"="BatchNum"))

batch4<-splitraw %>% 
  group_by(`Boar Stud`,BatchNum) %>% 
  summarize('Number of Boars'=n_distinct(BoarID))

batch5<-left_join(x = batch4,y = batch3,by=c("Boar Stud"="Boar Stud","BatchNum"="BatchNum"))

batch6<-batch5 %>% 
  filter(Date_Shipped<floor_date(x = today(), unit = "week", week_start = 1),
         Date_Shipped>=floor_date(x = today(), unit = "week", week_start = 1)-7)

batch7<-batch6 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Total Batches'=n_distinct(BatchNum))

batch8<-batch6 %>% 
  filter(`Number of Boars`==2) %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Two Boars'=n_distinct(BatchNum))

batch9<-batch6 %>% 
  filter(`Number of Boars`>6) %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Seven or More Boars'=n_distinct(BatchNum))

batch10<-left_join(x = batch7,y = batch8,by=c("Boar Stud"="Boar Stud"))
batch11<-left_join(x = batch10,y = batch9,by=c("Boar Stud"="Boar Stud"))

write_csv(x = batch11,path = here::here("data","batch.csv"),append = FALSE)

batch12<-batch6 %>% 
  filter(`Number of Boars`==2 | `Number of Boars`>6)

batch12<-batch12[c(1,8,3,5,6)]

batch12<-batch12[!duplicated(batch12),]

write_csv(x = batch12,path = here::here("data","batch1.csv"),append = FALSE)
