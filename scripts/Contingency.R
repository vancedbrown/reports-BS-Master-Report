library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

collraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/coll.csv")
collraw$Col_Date<-as.Date(collraw$Col_Date)

breakdown <- read_csv(file = here::here("data","breakdown.csv"))

cont1<-breakdown %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  filter(Collected=='YES',
         `Collection Status`=='US',
         Distributed=='NO')

cont2<-breakdown %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  filter(Collected=='NO',
         `Boar Status`=='WORKING')

cont3<-left_join(x = cont1,y = collraw,by=c("BoarID"="BoarID","Col_Date"="Col_Date"))

cont4<-left_join(x = cont2,y = collraw,by=c("BoarID"="BoarID"))

cont5<-cont4 %>% 
  group_by(BoarID) %>% 
  filter(!is.na(Col_Date.y),
        `Collection Status.y`!='NC',
         Col_Date.y==max(Col_Date.y))

cont6<-cont5 %>% 
  group_by(BoarID) %>% 
  filter(Tot_Sperm==max(Tot_Sperm))

cont7<-cont6 %>% 
  group_by(`Boar Stud.x`,Breed.x) %>% 
  summarise(a=sum(Tot_Sperm))

cont8<-cont3 %>% 
  group_by(BoarID) %>% 
  filter(Tot_Sperm==max(Tot_Sperm))

cont9<-cont8 %>% 
  group_by(`Boar Stud.x`,Breed.x) %>% 
  summarise(b=sum(Tot_Sperm))

cont10<-full_join(x = cont7,y = cont9,by=c("Boar Stud.x"="Boar Stud.x","Breed.x"="Breed.x"))

cont10[is.na(cont10)]<-0

cont10$sperm<-cont10$a+cont10$b

write_csv(x = cont10, path = here::here("data","cont.csv"), append = FALSE)
         