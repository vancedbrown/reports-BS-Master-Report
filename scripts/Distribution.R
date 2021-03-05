library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

distraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/dist.csv")

distraw$Date_Shipped<-as.Date(distraw$Date_Shipped)

dist1<-distraw %>% 
  group_by(`Boar Stud`) %>% 
  filter(Date_Shipped<floor_date(x = today(), unit = "week", week_start = 1),
         Date_Shipped>=floor_date(x = today(), unit = "week", week_start = 1)-7,
         Dest!='* TRASH *') %>% 
  summarise('Doses Distributed'=sum(Doses))

write_csv(x = dist1, path = here::here("data","dist1.csv"), append = FALSE)

