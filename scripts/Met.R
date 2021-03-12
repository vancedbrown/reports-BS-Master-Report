library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")
collraw$Col_Date<-as.Date(collraw$Col_Date)

met1<-collraw %>% 
  filter(Col_Date<floor_date(x = today(), unit = "week", week_start = 1),
         Col_Date>=floor_date(x = today(), unit = "week", week_start = 1)-7,
         !is.na(Sperm_Conc))

met2<-met1 %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Number of Readings'=n())

met3<-met1 %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Average Reading'=mean(Sperm_Conc))

met4<-met1 %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Standard Deviation'=sd(Sperm_Conc))

met5<-met1 %>% 
  group_by(`Boar Stud`) %>% 
  filter(Sperm_Conc<0.1) %>% 
  summarise('Readings Under 0.100'=n())

met6<-met1 %>% 
  group_by(`Boar Stud`) %>% 
  filter(Sperm_Conc>0.3) %>% 
  summarise('Readings Over 0.300'=n())

met7<-left_join(x = met2,y = met3,by=c("Boar Stud"="Boar Stud"))
met8<-left_join(x = met7,y = met4,by=c("Boar Stud"="Boar Stud"))
met9<-left_join(x = met8,y = met5,by=c("Boar Stud"="Boar Stud"))
met10<-left_join(x = met9,y = met6,by=c("Boar Stud"="Boar Stud"))

met10[is.na(met10)]<-0

met11<-met10 %>% 
  filter(`Boar Stud`%in%c('High Desert',
                          'MB 7081',
                          'MB 7082',
                          'MB 7092',
                          'MB 7093',
                          'MB 7094',
                          'MBW Cimarron',
                          'MBW Cyclone',
                          'MBW Yuma',
                          'Princeton',
                          'Skyline Boar Stud',
                          'SPGNC',
                          'SPGVA',
                          'SPGTX'))
  

write_csv(x = met11,path = here::here("data","met.csv"),append = FALSE)
