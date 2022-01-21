library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(here)

hour1<-read_csv(here::here("inputs","hours.csv"),
                col_types = cols(`Pay Per Date` = col_date(format = "%m/%d/%Y")))

hour2<-read_csv(here::here("data","pcarddata.csv"))

hour2$Date<-as.Date(hour2$WeekCommencing, origin = "1970-01-01")

hour3<-hour2[c(1,2,44)]

hour1$ppstart<-as.Date(hour1$`Pay Per Date`)

hour4<-hour1 %>% 
  group_by(`Employee Home Business Unit`,`Pay Per Date`) %>% 
  filter(`Pay Type`%in%c(1,12),
         !is.na(Hours)) %>% 
  summarise('Total Hours'=sum(Hours))

hour4$ppstart<-hour4$`Pay Per Date`-13

hour5<-read_csv(here::here("inputs","link.csv"))

hour6<-left_join(x = hour3,y = hour5,by=c("Boar Stud"="Stud"))

hour7<-full_join(x = hour6,y = hour4,by=c("CC"="Employee Home Business Unit"))

hour8<-hour7 %>% 
  group_by(`Boar Stud`) %>% 
  filter(ppstart==Date | ppstart==Date-7)

hour9<-hour8 %>% 
  group_by(`Boar Stud`,`Pay Per Date`) %>% 
  summarise('Hours/Boar/Week'=mean(`Total Hours`)/sum(Inventory))

hour10<-hour9 %>% 
  group_by(`Boar Stud`) %>% 
  filter(`Boar Stud`!='SPGTX') %>% 
  top_n(`Pay Per Date`,n = 6) %>% 
  spread(key = `Pay Per Date`,value = `Hours/Boar/Week`)

write_csv(x = hour10,path = here::here("data","hours.csv"), append = FALSE)
