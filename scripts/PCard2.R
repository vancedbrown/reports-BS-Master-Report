library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(here)

pcard1<- read_csv(here::here("data","14day.csv"))
pcard2<- read_csv(here::here("data","mort2.csv"))
pcard3<- read_csv(here::here("data","ppm.csv"))
pcard4<-read_csv(here::here("data","treatments.csv"))
pcard5<- read_csv(here::here("data","inventory.csv"))
pcard6<-read_csv(here::here("data","topn.csv"))
pcard7<-read_csv(here::here("data","lut.csv"))

pcard8<-pcard7[c(1:4)]
pcard2<-pcard2[c(1,2,5,6)]

pcard6$WeekCommencing=as.Date(pcard6$WeekCommencing)

pcard6$yrwk<-(year((pcard6$WeekCommencing)+5)*100)+isoweek((pcard6$WeekCommencing)+5)

pcard9<-left_join(x = pcard5,y = pcard2,by=c("Boar Stud"="Boar Stud"))
pcard10<-left_join(x = pcard9,y = pcard1,by=c("Boar Stud"="Boar Stud.x"))
pcard11<-left_join(x = pcard10,y = pcard3,by=c("Boar Stud"="Boar Stud"))
pcard12<-left_join(x = pcard11,y = pcard4,by=c("Boar Stud"="Boar Stud"))
pcard13<-left_join(x = pcard12,y = pcard6,by=c("Boar Stud"="Boar Stud"))
pcard14<-left_join(x = pcard13,y = pcard8,by=c("Boar Stud"="Boar Stud"))

pcard15<-pcard14 %>% 
  filter(!is.na(WeekCommencing))

pcard15[is.na(pcard15)]<-0

pcard15$scores<-pcard15$Batches*6
pcard15$`% Missing Scores`<-(pcard15$`Missing Scores`/pcard15$scores)*100
pcard15$`% 14 Day Rest`<-(pcard15$`14 Day Rest`/pcard15$Inventory)*100


pcard16<-pcard15 %>% 
  mutate(TScore=ifelse(`Treatment Rate`>0,3,0),
         RScore=ifelse(`% 14 Day Rest`<=2,3,
                       ifelse(`% 14 Day Rest`<=4,2,
                              ifelse(`% 14 Day Rest`<=6,1,0))),
         PScore=ifelse(is.na(`% Missing Scores`),2,
                       ifelse(`% Missing Scores`<=1,2,0)),
         LScorea=ifelse(`Lutalyse Usage`<=5,2,
                        ifelse(`Lutalyse Usage`<=10,1,0)),
         LScoreb=ifelse(`Lutalyse Usage`==0,0,LScorea),
         LScore=ifelse(`Boar Stud`%in%c('Skyline Boar Stud','Norson','SPGVA','SPGNC'),2,LScoreb),
         Total=TScore+RScore+PScore+LScore)

# write_csv(x = pcard16,path = here::here("data","pcarddata2.csv"))

pcard17<-read_csv(here::here("data","pcarddata2.csv"))

pcard18<-rbind(pcard17,pcard16)

write_csv(x = pcard18, path = here::here("data","pcarddata2.csv"),append = FALSE)

pcard19<-pcard18 %>%
  group_by(`Boar Stud`) %>%
  top_n(5,yrwk) %>% 
  filter(yrwk!=max(yrwk)) %>% 
  summarise(`4 Week Average`=mean(Total))

pcard20<-pcard18 %>% 
  group_by(`Boar Stud`) %>% 
  top_n(13,yrwk) %>% 
  summarise(`Quarter Average`=mean(Total))

pcard21<-left_join(x = pcard16,y = pcard19,by=c("Boar Stud"="Boar Stud"))
pcard22<-left_join(x = pcard21,y = pcard20,by=c("Boar Stud"="Boar Stud"))

pcard22<-pcard22[c(1,20,32,31,33,30,34,28,37,38,39,40)]


write_csv(x = pcard22,path = here::here("data","pcard2.csv"),append = FALSE)
