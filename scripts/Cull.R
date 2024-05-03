library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv", 
                 col_types = cols(Index = col_number()))

lookraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/look.csv")
lookraw$key<-paste(lookraw$`Boar Stud`,lookraw$ID)


pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)

cull1<-pigraw %>% 
  filter(is.na(Date_Studout))

cull2<-pigraw %>% 
  filter(Date_Studout<floor_date(x = today(),unit = "week", week_start = 7),
         Date_Studout>=floor_date(x = today(),unit= "week", week_start = 7)-7)

cull3<-rbind(cull1,cull2)

cull4<-cull3 %>% 
  group_by(`Boar Stud`,Breed) %>% 
  mutate(rank=row_number(-Index),
         ranksum=max(rank))

cull5a<-cull4 %>% 
  group_by(`Boar Stud`,Breed) %>% 
  mutate(Percentile=rank/ranksum)


cull5b<-cull5a %>% 
  mutate(key=paste(`Boar Stud`,Dispose_Code))

cull5<-left_join(x = cull5b,y = lookraw,by=c("key"="key"))

cull6<-cull5 %>% 
  group_by(`Boar Stud.x`) %>% 
  filter(`Boar Status`=='CULLED') %>% 
  summarise('Boars Culled'=n_distinct(BoarID))

cull6a<-cull5 %>% 
  filter(`Boar Status`=='CULLED') %>% 
  mutate(WeekCommencing=floor_date(x = today(),unit= "week", week_start = 7)-7)

write_csv(x = cull6a, file = here::here("data","cullboars.csv"),append = TRUE, col_names = FALSE)

cull7<-cull5 %>% 
  group_by(`Boar Stud.x`) %>% 
  filter(`Boar Status`=='CULLED',
         Percentile>=0.75 | DESCR=='GENETIC SERVICES') %>% 
  summarise('Low Index Culls'=n_distinct(BoarID))

cull8<-left_join(x = cull6,y = cull7,by=c("Boar Stud.x"="Boar Stud.x"))

cull8[is.na(cull8)]<-0

cull8$WeekCommencing<-floor_date(x = today(),unit= "week", week_start = 7)-7
cull8$WeekCommencing<-format(cull8$WeekCommencing, "%m/%d/%Y")


write_csv(x = cull8,path = here::here("data","cullopen.csv"),append = TRUE, col_names = FALSE)
write_csv(x = cull8,path = here::here("data","cull.csv"),append = TRUE, col_names = FALSE)

cull9<-cull5 %>% 
  filter(`Boar Status`%in%c('CULLED','DEAD'))

cull10<-cull9

cull10$week<-floor_date(x = today(),unit = "week",week_start = 1)-7

cull11<-cull10 %>% 
  filter(!BoarID%in%c('16198','16017'),
         DESCR=='LOW INDEX',
         Percentile<=0.75)

write_csv(x = cull11,path = here::here("data","indexcullerrorboars.csv"), append = TRUE)

cull12<-cull11 %>% 
  group_by(`Boar Stud.x`) %>% 
  summarise('Cull Errors'=n_distinct(BoarID))

cull12$week<-floor_date(x = today(),unit = "week",week_start = 1)-7

write_csv(x = cull12,path = here::here("data","cullerrors.csv"),append = TRUE)

### SQ and TC cull errors ###

cull13<-pigraw %>% 
  filter(Date_Studout<floor_date(x = today(),unit = "week", week_start = 7),
         Date_Studout>=floor_date(x = today(),unit= "week", week_start = 7)-28)


cull14<-cull13 %>% 
  mutate(key=paste(`Boar Stud`,Dispose_Code))

cull15<-left_join(x = cull14,y = lookraw,by=c("key"="key"))
 
cull16<- read_csv(here::here("data","semenculls.csv"))  
cull17<- read_csv(here::here("data","trainingculls.csv"))

cull17$BoarID<-as.character(cull17$BoarID)
cull16$BoarID<-as.character(cull16$BoarID)

cull18<-left_join(x = cull15,y = cull16, by=c("BoarID"="BoarID"))
cull19<-left_join(x = cull18,y = cull17, by=c("BoarID"="BoarID"))

cull20<-cull19 %>% 
  filter(DESCR%in%c('WILL NOT TRAIN','SEMEN QUALITY'))

cull21<-cull20 %>% 
  filter(BoarID!='2097962',
         DESCR=='SEMEN QUALITY',
         `Collections Trashed Consecutively`<6 | is.na(`Collections Trashed Consecutively`)) %>% 
  group_by(`Boar Stud.x`) %>% 
  summarise('Semen Quality Cull Errors'=n_distinct(BoarID))

cull22<-cull20 %>% 
  filter(DESCR=='WILL NOT TRAIN',
         `Consecutive Failed Collections`<8 | is.na(`Consecutive Failed Collections`)) %>% 
  group_by(`Boar Stud.x`) %>% 
  summarise('Will Not Train Errors'=n_distinct(BoarID))

write_csv(x = cull20,file = here::here("data","sqntboars.csv"), append = FALSE)
write_csv(x = cull21, file = here::here("data","sqerrors.csv"), append = FALSE)
write_csv(x = cull22, file = here::here("data","nterrors.csv"), append = FALSE)

