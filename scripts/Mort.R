library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv")
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

mort27<-pigraw %>% 
  filter(`Boar Status`=='DEAD') %>% 
  filter(Date_Studout>=today()-91) %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Total Dead'=n_distinct(BoarID))

mort28<-left_join(x = mort26,y = mort27,by=c("Boar Stud"="Boar Stud"))

mort28[is.na(mort28)]<-0

mort29<-mort28 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Annualized Mortality'=((`Total Dead`*4)/`Average Inventory`)*100)

mort30<-left_join(x = mort28,y = mort29,by=c("Boar Stud"="Boar Stud"))

write_csv(x = mort30,path = here::here("data","mort.csv"),append = FALSE)
