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
pcard4<-read_csv(here::here("data","trash.csv"))
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
  mutate(MScore=ifelse(`% Non-Genetic Removals`<=15,3,
                       ifelse(`% Non-Genetic Removals`-15<=15,2,
                              ifelse(`% Non-Genetic Removals`-15<=30,1,0))),
         PScore=ifelse(`% Missing Scores`<=2,3,
                       ifelse(`% Missing Scores`<=4,2,
                              ifelse(`% Missing Scores`<=6,1,0))),
         RScore=ifelse(`% 14 Day Rest`<=5,3,
                       ifelse(`% 14 Day Rest`<=10,2,
                              ifelse(`% 14 Day Rest`<=15,1,0))),
         CScore=ifelse(as.integer(`Trash Rate`)%in%c(5:15),3,
                       ifelse(as.integer(`Trash Rate`)%in%c(2:5,15:25),2,
                              ifelse(as.integer(`Trash Rate`)%in%c(1:2,25:35),1,0))),
         LScorea=ifelse(`Lutalyse Usage`<=5,3,
                        ifelse(`Lutalyse Usage`<=10,2,
                               ifelse(`Lutalyse Usage`<=15,1,0))),
         LScoreb=ifelse(`Lutalyse Usage`==0,0,LScorea),
         LScore=ifelse(`Boar Stud`%in%c('Skyline Boar Stud','Norson','SPGVA'),3,LScoreb),
         Total=MScore+PScore+RScore+CScore+LScore)

pcard16$wk<-isoweek(pcard16$WeekCommencing)
pcard16$qt<-ifelse(pcard16$wk%in%c(1:13),1,
                  ifelse(pcard16$wk%in%c(14:26),2,
                         ifelse(pcard16$wk%in%c(27:39),3,4)))
pcard16$yr<-year((pcard16$WeekCommencing+5))

#write_csv(x = pcard16,path = here::here("data","pcarddata.csv"))

pcard17<-read_csv(here::here("data","pcarddata.csv"))
pcard18<-read_csv(here::here("inputs","months.csv"))
pcard18$Week<-as.numeric(pcard18$Week)

pcard17<-pcard17[-c(43)]

pcard19<-rbind(pcard17,pcard16)

pcard20<-left_join(x = pcard19,y = pcard18, by=c("yrwk"="Week"))

write_csv(x = pcard20, path = here::here("data","pcarddata.csv"),append = FALSE)

pcard21<-pcard19 %>%
  group_by(`Boar Stud`) %>%
  top_n(5,yrwk) %>% 
  filter(yrwk!=max(yrwk)) %>% 
  summarise(`4 Week Average`=mean(Total))

pcard22<-pcard19 %>% 
  group_by(`Boar Stud`) %>% 
  top_n(13,yrwk) %>% 
  summarise(`Quarter Average`=mean(Total))

pcard23<-left_join(x = pcard16,y = pcard21,by=c("Boar Stud"="Boar Stud"))
pcard24<-left_join(x = pcard23,y = pcard22,by=c("Boar Stud"="Boar Stud"))

pcard24<-pcard24[c(1,7,32,30,33,31,34,20,35,28,38,39,43,44)]

#pcard21<-pcard21

write_csv(x = pcard24,path = here::here("data","pcard.csv"),append = FALSE)
