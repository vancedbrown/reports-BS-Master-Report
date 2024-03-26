library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(here)

scard1<-read_csv("//ncrh1/users/Vance/Semen QC IVOS Data/Semen_QC_Master.csv")
scardmatch<-read_csv(file = here::here("inputs","match.csv"))

scard1$yearweek<-(scard1$Year*100)+scard1$Week

scard2<-scard1 %>%
  group_by(StudName, yearweek) %>% 
  summarise('Minimum Concentration'=min(Concentration),
            'Average Concentration'=mean(Concentration),
            'Minimum Morphology'=min(`%Normal_Morph`),
            'Samples'=n())

scard3<-scard1 %>% 
  group_by(StudName,yearweek) %>% 
  filter(Bacteria_CFUs>3) %>% 
  summarise('Positive Samples'=n())

scard4<-left_join(x = scard2, y = scard3, by=c("StudName"="StudName", "yearweek"="yearweek"))

scard4[is.na(scard4)]<-0

scard4$`Bacteria %`<-(scard4$`Positive Samples`/scard4$Samples)*100
scard4$`Conc_Diff`<-abs(scard4$`Average Concentration`-44.0)

scard5<-scard4 %>% 
  mutate('CScore'=ifelse(`Minimum Concentration`>=40.0,4,
                         ifelse(`Minimum Concentration`>=39.0,3,
                                ifelse(`Minimum Concentration`>=38.0,2,
                                       ifelse(`Minimum Concentration`>=37.0,1,0)))),
         'MScore'=ifelse(`Minimum Morphology`>=70.0,3,
                         ifelse(`Minimum Morphology`>=65.0,2,
                                ifelse(`Minimum Morphology`>=60.0,1,0))),
         'AScore'=ifelse(Conc_Diff<=3.0,2,
                         ifelse(Conc_Diff<=6.0,1,0)),
         'BScore'=ifelse(`Bacteria %`==0,1,0),
         'Total'=CScore+MScore+AScore+BScore)

scard6<-scard5 %>% 
  group_by(StudName) %>% 
  slice_max(order_by = yearweek, n = 5, with_ties = TRUE) %>% 
  filter(yearweek!=max(yearweek)) %>% 
  summarise('4 Week Average'=mean(Total))

scard7<-scard5 %>% 
  group_by(StudName) %>% 
  slice_max(order_by = yearweek, n = 13, with_ties = TRUE) %>% 
  summarise('13 Week Average'=mean(Total))

scard8<-scard5 %>% 
  ungroup() %>% 
  filter(yearweek==(year(today()-7)*100+isoweek(today()-7)))

scard9<-left_join(x = scard7,y = scard6,by=c("StudName"="StudName"))
scard10<-left_join(x = scard9,y = scard8,by=c("StudName"="StudName"))
scard11a<-left_join(x = scard10,y = scardmatch, by=c("StudName"="Stud"))

scard12<-left_join(x = scard5,y = scardmatch,by=c("StudName"="Stud"))

scard11<-scard11a %>% 
  filter(!is.na(`Boar Stud`))

scard11<-scard11[c(17,8,5,12,7,13,6,14,10,15,16,3,2)]

write_csv(x = scard12,file = here::here("data","scarddata2.csv"))
write_csv(x = scard11,file = here::here("data","scard2.csv"))
