library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv",
                 col_types = cols(Index = col_number()))
lookraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/look.csv")

pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)


mort1<-pigraw %>% 
  filter(Date_Arrival<=today()-91) %>% 
  filter(Date_Studout>=today()-91 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n1=n_distinct(BoarID))

mort2<-pigraw %>% 
  filter(Date_Arrival<=today()-84) %>% 
  filter(Date_Studout>=today()-84 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n2=n_distinct(BoarID))

mort3<-pigraw %>% 
  filter(Date_Arrival<=today()-77) %>% 
  filter(Date_Studout>=today()-77 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n3=n_distinct(BoarID))

mort4<-pigraw %>% 
  filter(Date_Arrival<=today()-70) %>% 
  filter(Date_Studout>=today()-70 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n4=n_distinct(BoarID))

mort5<-pigraw %>% 
  filter(Date_Arrival<=today()-63) %>% 
  filter(Date_Studout>=today()-63 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n5=n_distinct(BoarID))

mort6<-pigraw %>% 
  filter(Date_Arrival<=today()-56) %>% 
  filter(Date_Studout>=today()-56 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n6=n_distinct(BoarID))

mort7<-pigraw %>% 
  filter(Date_Arrival<=today()-49) %>% 
  filter(Date_Studout>=today()-49 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n7=n_distinct(BoarID))

mort8<-pigraw %>% 
  filter(Date_Arrival<=today()-42) %>% 
  filter(Date_Studout>=today()-42 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n8=n_distinct(BoarID))

mort9<-pigraw %>% 
  filter(Date_Arrival<=today()-35) %>% 
  filter(Date_Studout>=today()-35 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n9=n_distinct(BoarID))

mort10<-pigraw %>% 
  filter(Date_Arrival<=today()-28) %>% 
  filter(Date_Studout>=today()-28 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n10=n_distinct(BoarID))

mort11<-pigraw %>% 
  filter(Date_Arrival<=today()-21) %>% 
  filter(Date_Studout>=today()-21 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n11=n_distinct(BoarID))

mort12<-pigraw %>% 
  filter(Date_Arrival<=today()-14) %>% 
  filter(Date_Studout>=today()-14 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n12=n_distinct(BoarID))

mort13<-pigraw %>% 
  filter(Date_Arrival<=today()-7) %>% 
  filter(Date_Studout>=today()-7 | is.na(Date_Studout)) %>% 
  group_by(`Boar Stud`) %>% 
  summarize(n13=n_distinct(BoarID))

mort14<- left_join(x = mort1,y = mort2,by=c("Boar Stud"="Boar Stud"))
mort15<- left_join(x = mort14,y = mort3,by=c("Boar Stud"="Boar Stud"))
mort16<- left_join(x = mort15,y = mort4,by=c("Boar Stud"="Boar Stud"))
mort17<- left_join(x = mort16,y = mort5,by=c("Boar Stud"="Boar Stud"))
mort18<- left_join(x = mort17,y = mort6,by=c("Boar Stud"="Boar Stud"))
mort19<- left_join(x = mort18,y = mort7,by=c("Boar Stud"="Boar Stud"))
mort20<- left_join(x = mort19,y = mort8,by=c("Boar Stud"="Boar Stud"))
mort21<- left_join(x = mort20,y = mort9,by=c("Boar Stud"="Boar Stud"))
mort22<- left_join(x = mort21,y = mort10,by=c("Boar Stud"="Boar Stud"))
mort23<- left_join(x = mort22,y = mort11,by=c("Boar Stud"="Boar Stud"))
mort24<- left_join(x = mort23,y = mort12,by=c("Boar Stud"="Boar Stud"))
mort25<- left_join(x = mort24,y = mort13,by=c("Boar Stud"="Boar Stud"))

mort25[is.na(mort25)]<-0

mort26<-mort25 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Average Inventory'=mean(c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13)))

write_csv(x = mort26,path = here::here("data","avginv.csv"), append = FALSE)


lookraw$key<-paste(lookraw$`Boar Stud`,lookraw$ID)

mort27<-pigraw %>% 
  filter(`Boar Status`%in%c('DEAD','CULLED')) %>% 
  filter(Date_Studout>=floor_date(x = today(),unit = "week",week_start = 1)-91) %>% 
  mutate(key=paste(`Boar Stud`,Dispose_Code))

mort28<-left_join(x = mort27,y = lookraw,by=c("key"="key"))

write_csv(mort28,path = here::here("data","ngr.csv"), append = FALSE)

mort29<-mort28 %>% 
  filter(DESCR%in%c('DIED','FEETANDLEGS','WILL NOT TRAIN') | is.na(DESCR)) %>% 
  group_by(`Boar Stud.x`) %>% 
  summarise('Non-Genetic Removals'=n_distinct(BoarID))

mort30<-left_join(x = mort26,y = mort29,by=c("Boar Stud"="Boar Stud.x"))

mort30[is.na(mort30)]<-0

mort31<-read_csv(here::here("data","cullerrors.csv"))

mort32<-mort31 %>% 
  group_by(`Boar Stud.x`) %>% 
  filter(week>=floor_date(x = today(),unit = "week",week_start = 1)-91) %>% 
  summarise("Index Cull Errors"=sum(`Cull Errors`))

mort33<-left_join(x = mort30,y = mort32,by=c("Boar Stud"="Boar Stud.x"))
  
mort33[is.na(mort33)]<-0

mort33$`Total Non-Genetic Removals`<-mort33$`Non-Genetic Removals`+mort33$`Index Cull Errors`

mort34<-mort33 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('% Non-Genetic Removals'=((`Total Non-Genetic Removals`*4)/`Average Inventory`)*100)

mort35<-left_join(x = mort33,y = mort34,by=c("Boar Stud"="Boar Stud"))

mort36<-mort35 %>% 
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

write_csv(x = mort36,path = here::here("data","mort2.csv"),append = FALSE)
