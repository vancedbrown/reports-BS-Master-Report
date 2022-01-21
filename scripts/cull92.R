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


source('C:/Users/vance/Documents/myR/functions/getSQL.r')

query1<-"SELECT a.StudID
      ,a.BoarID
	  ,b.DV_IDX
	  ,b.Product
  FROM Intranet.dbo.Boar_Pig a
  inner join Intranet.dbo.ProductIdx b on a.BoarID = b.SPGid
  WHERE a.StudID = 'MB 7092' and b.Product = 'D1_KOK'"
kokindex<-getSQL('Intranet',query = query1)


culla1<-pigraw %>% 
  filter(is.na(Date_Studout),
         `Boar Stud`=='MB 7092')

culla2<-pigraw %>% 
  filter(`Boar Stud`=='MB 7092',
         Date_Studout<floor_date(x = today(),unit = "week", week_start = 7),
         Date_Studout>=floor_date(x = today(),unit= "week", week_start = 7)-7)

culla3<-rbind(culla1,culla2)


culla4a<-left_join(x = culla3,y = kokindex, by=c("BoarID"="BoarID"))


culla4<-culla4a %>% 
  filter(DV_IDX<113.0) %>% 
  group_by(`Boar Stud`,Breed) %>% 
  mutate(rank=row_number(-Index),
         ranksum=max(rank))

culla5a<-culla4 %>% 
  group_by(`Boar Stud`,Breed) %>% 
  mutate(Percentile=rank/ranksum)


culla5b<-culla5a %>% 
  mutate(key=paste(`Boar Stud`,Dispose_Code))

culla5<-left_join(x = culla5b,y = lookraw,by=c("key"="key"))

culla6<-culla5 %>% 
  group_by(`Boar Stud.x`) %>% 
  filter(`Boar Status`=='CULLED') %>% 
  summarise('Boars Culled'=n_distinct(BoarID))

culla7<-culla5 %>% 
  group_by(`Boar Stud.x`) %>% 
  filter(`Boar Status`=='CULLED',
         Percentile>=0.75 | DESCR=='GENETIC SERVICES') %>% 
  summarise('Low Index Culls'=n_distinct(BoarID))

culla8<-left_join(x = culla6,y = culla7,by=c("Boar Stud.x"="Boar Stud.x"))

culla8[is.na(culla8)]<-0

culla8$WeekCommencing<-floor_date(x = today(),unit= "week", week_start = 7)-7
culla8$WeekCommencing<-as.Date(x = culla8$WeekCommencing, format='%mm/%dd/%YYYY')


write_csv(x = culla8,path = here::here("data","cullaopen.csv"),append = TRUE, col_names = FALSE)
write_csv(x = culla8,path = here::here("data","cull.csv"),append = TRUE, col_names = FALSE)

culla9<-culla5 %>% 
  filter(`Boar Status`%in%c('CULLED','DEAD'))

culla10<-culla9

culla10$week<-floor_date(x = today(),unit = "week",week_start = 1)-7

culla11<-culla10 %>% 
  filter(DESCR=='LOW INDEX',
         Percentile<0.75)

write_csv(x = culla11,path = here::here("data","indexcullaerrorboars.csv"), append = TRUE)

culla12<-culla11 %>% 
  group_by(`Boar Stud.x`) %>% 
  summarise('Cull Errors'=n_distinct(BoarID))

write_csv(x = culla12,path = here::here("data","cullerrors.csv"),append = TRUE)

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