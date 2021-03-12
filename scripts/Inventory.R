library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(here)

pig<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv")

pig1<-pig %>% 
  filter(is.na(Date_Studout))

target<-read_csv(here::here("inputs","targets.csv"))

pig2<-pig1 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Inventory'=n_distinct(BoarID))

pig3<-left_join(x = pig2,y = target,by=c("Boar Stud"="StudID"))

pig3$`Inventory to target`<-(pig3$Inventory/pig3$Target)*100

pig4<-pig1 %>% 
  group_by(`Boar Stud`) %>% 
  filter(`Boar Status`=="WORKING") %>% 
  summarize('Working Boars'=n_distinct(BoarID))

pig5<-pig1 %>% 
  group_by(`Boar Stud`) %>% 
  filter(`Boar Status`=="NONWORKING") %>% 
  summarize('Non-Working Boars'=n_distinct(BoarID))

write_csv(x = pig3, path = here::here("data","inventory.csv"), append = FALSE)

pig6<-left_join(x = pig3,y = pig4,by=c("Boar Stud"="Boar Stud"))
pig7<-left_join(x = pig6,y = pig5,by=c("Boar Stud"="Boar Stud"))
pig7[is.na(pig7)]<-0
pig7<-pig7[c(1,2,3,5,6)]

write_csv(x = pig7, path = here::here("data","inventoryex.csv"), append = FALSE)
     