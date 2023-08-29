library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(here)


pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv")
collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")
trtraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/trt.csv")

collraw$Col_Date<-as.Date(collraw$Col_Date)
pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)


lut1<-trtraw %>% 
  mutate(t=ifelse(TCode=='LUT','LUT',
                  ifelse(TCode=='LUTA','LUT',
                         ifelse(TCode=='055','LUT','OTHER')))) %>% 
  filter(t=='LUT')

lut1$TDate<-as.Date(lut1$TDate)

lut2<-left_join(x = collraw,y = lut1,by=c("BoarID"="BoarID","Col_Date"="TDate"))

lut3<-lut2 %>% 
  mutate(week=floor_date(x = Col_Date,unit = "week",week_start = 1))

lut3<-lut3[order(lut3$BoarID,lut3$Col_Date),]

lut4<-lut3 %>%
  group_by(BoarID,`Boar Stud.x`) %>% 
  mutate(dr=row_number(Col_Date),
         count=rowsum(dr,group = BoarID))

# lut7aa<-lut4 %>% 
#   filter(`Collection Status`!='NC') %>% 
#   group_by(`Boar Stud.x`,week) %>% 
#   summarise('Collections'=n())

lut4a<-left_join(x = lut4,y = pigraw, by=c("BoarID"="BoarID"))
lut4b<-lut4a[c(1:5,10,23:25,28)]
write_csv(x = lut4b,file = here::here("data","all_lut.csv"))

lut4c<-lut4a %>% 
  filter(Breed=='SPG240')

lut5<-lut4c %>% 
  filter(dr>4)

lut5<-lut5[c(1,3,4,10,23,24)]
lut5$`Boar Stud`<-lut5$`Boar Stud.x`

lut6<-lut5 %>% 
  filter(t=='LUT') %>% 
  group_by(`Boar Stud`,week) %>% 
  summarise('Treatments'=n())

lut7<-lut5 %>% 
  filter(`Collection Status`!='NC') %>% 
  group_by(`Boar Stud`,week) %>% 
  summarise('Collections'=n())

lut8<-left_join(x = lut7,y = lut6,by=c("Boar Stud"="Boar Stud","week"="week"))

lut8[is.na(lut8)]<-0

lut8$`Lutalyse Usage`=(lut8$Treatments/lut8$Collections)*100

lut8$week<-ymd(lut8$week)

lut9<-lut8 %>%
  filter(week==floor_date(today(),unit = "week",week_start = 1)-7)

lut9<-lut9[-c(2)]

lut10<-lut8 %>% 
  filter(week>=floor_date(today(),unit = "week",week_start = 1)-35,
         week<floor_date(today(),unit = "week",week_start = 1)-7)

lut10$yrwk<-(year(lut10$week+5)*100)+isoweek(lut10$week+2)

lut10<-lut10[c(1,6,5)]

lut11<-lut10 %>%
  group_by(`Boar Stud`) %>% 
  spread(key = yrwk,value = `Lutalyse Usage`)

lut12<-left_join(x = lut9,y = lut11,by=c("Boar Stud"="Boar Stud"))

write_csv(x = lut12,path = here::here("data","lut.csv"),append = FALSE)

#### Non-Lutalyse Treatments ###

treat1<-trtraw %>% 
  filter(TCode=='MED')

treat2<-treat1 %>% 
  filter(TDate<floor_date(x = today(),unit = "week", week_start = 7),
         TDate>=floor_date(x = today(),unit= "week", week_start = 7)-91)

treat3<-treat2 %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Treated Boars'=n_distinct(BoarID)/13)

treat4<-pigraw %>% 
  group_by(`Boar Stud`) %>% 
  filter(`Boar Status`%in%c('WORKING','NONWORKING')) %>% 
  summarise('Total Boars in Stud'=n_distinct(BoarID))

treat5<-left_join(x = treat4,y = treat3, by=c("Boar Stud"="Boar Stud"))        

treat5[is.na(treat5)]<-0
treat5$`Treatment Rate`<-(treat5$`Treated Boars`/treat5$`Total Boars in Stud`)*100

write_csv(x = treat5, file=here::here("data","treatments.csv"), append = FALSE)
