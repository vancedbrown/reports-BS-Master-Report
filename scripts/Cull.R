library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/2019/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv", 
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

cull7<-cull5 %>% 
  group_by(`Boar Stud.x`) %>% 
  filter(`Boar Status`=='CULLED',
         Percentile>=0.75 | DESCR=='GENETIC SERVICES') %>% 
  summarise('Low Index Culls'=n_distinct(BoarID))

cull8<-left_join(x = cull6,y = cull7,by=c("Boar Stud.x"="Boar Stud.x"))

cull8[is.na(cull8)]<-0

cull8$WeekCommencing<-floor_date(x = today(),unit= "week", week_start = 7)-7
cull8$WeekCommencing<-as.Date(x = cull8$WeekCommencing, format='%mm/%dd/%YYYY')


write_csv(x = cull8,path = here::here("data","cullopen.csv"),append = TRUE, col_names = FALSE)
write_csv(x = cull8,path = here::here("data","cull.csv"),append = TRUE, col_names = FALSE)

cull9<-cull5 %>% 
  filter(`Boar Status`%in%c('CULLED','DEAD'))

cull10<-cull9

cull10$week<-floor_date(x = today(),unit = "week",week_start = 1)-7

cull11<-cull10 %>% 
  filter(DESCR=='LOW INDEX',
         Percentile<0.75)

write_csv(x = cull11,path = here::here("data","indexcullerrorboars.csv"), append = TRUE)

cull12<-cull11 %>% 
  group_by(`Boar Stud.x`) %>% 
  summarise('Cull Errors'=n_distinct(BoarID))

write_csv(x = cull12,path = here::here("data","cullerrors.csv"),append = TRUE)

# cull13<-cull10 %>% 
#   group_by(`Boar Stud.x`) %>% 
#   filter(DESCR=='GENETIC SERVICES') %>% 
#   summarise('Low Index Culls_B'=n_distinct(BoarID))
# 
# cull14<-left_join(x = cull8,y = cull13,by=c("Boar Stud"="Boar Stud.x"))
# cull14[is.na(cull14)]<-0
# 
# cull14$`Low Index Culls`<-cull14$`Low Index Culls_A`+cull14$`Low Index Culls_B`
# 
# cull15<-cull14[c(1,2,6,4)]

# write_csv(x = cull15,path = here::here("data","cull.csv"),append = TRUE, col_names = FALSE)
