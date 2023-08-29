library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv")
collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")

collraw$Col_Date<-as.Date(collraw$Col_Date)
pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)


train1<-pigraw %>% 
  filter(!is.na(Date_Arrival))

train2a <- left_join(x = train1,y = collraw,by=c("BoarID"="BoarID"))

train2a$`Boar Stud`<-train2a$`Boar Stud.x`

train2<-train2a  %>%
  filter(!BoarID%in%c('1831G','1838G','1841G','1852G','1865G','PIC208681968','PIC208683985','PIC208685890','PIC208689733',
  'PIC208689738','PIC208691053','PIC208691057','PIC208693286','PIC208693291','PIC208694236','PIC208696263','PIC208697303',
  'PIC208698011','PIC208698013','PIC208698014','PIC208698868','PIC208698883','PIC208698885','PIC208699912','PIC208699921',
  'PIC208699922','PIC208701296','PIC208701373','PIC208702264','PIC208703700','PIC208706915'))

train3<-train2 %>%
  group_by(`Boar Stud`) %>% 
  summarize('Last Arrival Date'=max(Date_Arrival))

train4<-train2 %>% 
  group_by(`Boar Stud`) %>% 
  filter(Date_Arrival==max(Date_Arrival))

train5<-train4 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Total Boars'=n_distinct(BoarID))

train6<-train4 %>%
  group_by(`Boar Stud`) %>% 
  filter(is.na(`Boar Stud.y`)) %>% 
  summarize('No Attempt'=n_distinct(BoarID))

train7<-train4 %>%
  filter(!is.na(`Boar Stud.y`)) %>% 
  filter(`Collection Status`=='NC')

train8<-train4 %>%
  filter(!is.na(`Boar Stud.y`)) %>% 
  filter(`Collection Status`!='NC')

train9<-anti_join(x = train7,y = train8,by=c("BoarID"="BoarID"))

train10<-train9 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('No Collection'=n_distinct(BoarID))

train11<-train4 %>%
  filter(!is.na(`Boar Stud.y`)) %>% 
  filter(`Collection Status`!='NC') %>% 
  group_by(BoarID,`Boar Stud`) %>% 
  summarize(Collections=n())

train12<-train11 %>% 
  filter(Collections<4)

train13<-train12 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Less Than 4 Collections'=n())

train14<-train11 %>% 
  filter(Collections>=4)

train15<-train14 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('More Than 4 Collections'=n())

train16 <- left_join(x = train3,y = train5,by=c("Boar Stud"="Boar Stud"))
train17<-left_join(x = train16,y = train6,by=c("Boar Stud"="Boar Stud"))
train18<-left_join(x = train17,y = train10,by=c("Boar Stud"="Boar Stud"))
train19<-left_join(x = train18,y = train13,by=c("Boar Stud"="Boar Stud"))
train20<-left_join(x = train19,y = train15,by=c("Boar Stud"="Boar Stud"))

order.load<-order(train20$`Last Arrival Date`)
train21<-train20[order.load,]
train21[is.na(train21)]<-0

train21$`Percent Trained`<-((train21$`Less Than 4 Collections`+train21$`More Than 4 Collections`)/train21$`Total Boars`)*100

write_csv(x = train21,path = here::here("data","train.csv"),append = FALSE)

train22a<-train2 %>%
  filter(Date_Arrival>=floor_date(x = today(),unit = "week",week_start = 1)-105,
         Date_Arrival<floor_date(x = today(),unit = "week",week_start = 1)-14)

train22b<-train2 %>% 
  filter(Date_Arrival<floor_date(x = today(),unit = "week",week_start = 1)-14) %>% 
  group_by(`Boar Stud.x`) %>% 
  filter(Date_Arrival==max(Date_Arrival))

train22<- rbind(train22a,train22b)

train23<-train22 %>%
  group_by(`Boar Stud`) %>% 
  summarise('Total Boars'=n_distinct(BoarID))

train24<-train22 %>%
  group_by(`Boar Stud`) %>% 
  filter(`Collection Status`!='NC') %>% 
  mutate(cutoff=Date_Arrival+14,
         train=ifelse(Col_Date<=cutoff,1,0))

write_csv(x = train24,path = here::here('data','2wktrain.csv'))
write_csv(x = train22, path = here::here('data','allboars.csv'))

train25<-train24 %>% 
  group_by(`Boar Stud`) %>% 
  filter(train==1) %>% 
  summarise('Boars Trained Within 14 Days'=n_distinct(BoarID))

train26<-left_join(x = train23,y = train25,by=c("Boar Stud"="Boar Stud"))

train26$`Percent Trained`<-(train26$`Boars Trained Within 14 Days`/train26$`Total Boars`)*100

write_csv(x = train26,path = here::here("data","trained.csv"),append = FALSE)


###Recalculate on 13 weeks###

# rtrain1<-read_csv(here::here("data","weekscull.csv"),
#                   col_types = cols(Reportwk = col_date(format = "%m/%d/%Y")))
# 
# rtrain2<-train22 %>% 
#   group_by(`Boar Stud.x`, Date_Arrival) %>% 
#   summarise(tb=n_distinct(BoarID))
# 
# rtrain3<-train24 %>% 
#   group_by(`Boar Stud.x`, Date_Arrival) %>% 
#   filter(train==1) %>% 
#   summarise(bt=n_distinct(BoarID))
# 
# rtrain4<-left_join(x = rtrain2,y = rtrain3, by=c("Boar Stud.x"="Boar Stud.x","Date_Arrival"="Date_Arrival"))
# 
# rtrain4[is.na(rtrain4)]<-0
# 
# rtrain5<-left_join(x = rtrain1,y = rtrain4, by=c("Boar Stud"="Boar Stud.x"))
# 
# rtrain6<-rtrain5 %>% 
#   filter(Date_Arrival>=Reportwk-105,
#          Date_Arrival<=Reportwk-14)
# 
# rtrain7<-rtrain6 %>% 
#   group_by(`Boar Stud`,Reportwk) %>% 
#   summarise(tbs=sum(tb))
# 
# rtrain8<-rtrain6 %>% 
#   group_by(`Boar Stud`,Reportwk) %>% 
#   summarise(bts=sum(bt))
# 
# rtrain9<-left_join(x = rtrain7,y = rtrain8,by=c("Boar Stud"="Boar Stud","Reportwk"="Reportwk"))
# 
# write_csv(x = rtrain9,file = here::here("data","newtrain.csv"))
