library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(here)

gcard0<-read_csv(here::here("data","topn_nuc.csv"))
gcard1<- read_csv(here::here("data","inventory.csv"))
gcard2<-read_csv(here::here("data","topn.csv"))
gcard3<-read_csv(here::here("data","rest.csv"))
gcard4<-read_csv(here::here("data","cull.csv"))
gcard5<-read_csv(here::here("data","breakdown.csv"))
gcard6<-read_csv(here::here("data","trained.csv"))
gcard7<-read_csv(here::here("data","sqerrors.csv"),col_types = cols(`Semen Quality Cull Errors` = col_number()))
gcard8<-read_csv(here::here("data","nterrors.csv"),col_types = cols(`Will Not Train Errors` = col_number()))
gcard9<-read_csv(here::here("data","cullerrors.csv"))

gcard10<-gcard9 %>% 
  filter(week>=floor_date(x = today(),unit = "week",week_start = 1)-28) %>%
  group_by(`Boar Stud.x`) %>% 
  summarise("Index Cull Errors"=sum(`Cull Errors`))

gcard2$WeekCommencing<-as.Date(gcard2$WeekCommencing)
gcard4$WeekCommencing<-as.Date(gcard4$WeekCommencing, format="%m/%d/%Y")
gcard2$yrwk<-(year((gcard2$WeekCommencing)+5)*100)+isoweek((gcard2$WeekCommencing)+5)

gcard0$WeekCommencing<-as.Date(gcard0$WeekCommencing)
gcard0$yrwk<-(year((gcard0$WeekCommencing)+5)*100)+isoweek((gcard0$WeekCommencing)+5)

gcard2a<-gcard2 %>% 
  filter(!`Boar Stud`%in%c('SPGNC','SPGVA'))

gcard2b<-rbind(gcard2a,gcard0)

gcard11<-left_join(x = gcard1,y = gcard2b,by=c("Boar Stud"="Boar Stud"))
gcard12<-left_join(x = gcard11,y = gcard3, by=c("Boar Stud"="Boar Stud"))
gcard13<-left_join(x = gcard12,y = gcard6,by=c("Boar Stud"="Boar Stud"))
gcard14<-left_join(x = gcard13,y = gcard7, by=c("Boar Stud"="Boar Stud.x"))
gcard15<-left_join(x = gcard14, y = gcard8, by=c("Boar Stud"="Boar Stud.x"))
gcard16<-left_join(x = gcard15,y = gcard10, by=c("Boar Stud"="Boar Stud.x"))

gcard17<-gcard16 %>% 
  filter(!is.na(WeekCommencing))

gcard17[is.na(gcard17)]<-0

gcard18<-gcard17 %>% 
  group_by(`Boar Stud`) %>% 
  mutate('Removal Reason Errors'=sum(`Semen Quality Cull Errors`,`Will Not Train Errors`,`Index Cull Errors`))


gcard19<-gcard18 %>% 
  mutate(IScore=ifelse(`Missed Index`<=0.10,4,
                       ifelse(`Missed Index`<=0.20,3,
                              ifelse(`Missed Index`<=0.30,2,
                                     ifelse(`Missed Index`<=0.40,1,0)))),
         TScore=ifelse(`Total Boars`<=10,3,
                       ifelse(`Percent Trained`==0,3,
                              ifelse(`Percent Trained`>=90,3,
                                     ifelse(`Percent Trained`>=80,2,
                                            ifelse(`Percent Trained`>=70,1,0))))),
         RScore=ifelse(abs(`Average Days Rest`-7)<=1,2,
                       ifelse(abs(`Average Days Rest`-7)<=2,1,0)),
         CScore=ifelse(`Removal Reason Errors`<=0,1,0),
         Total=IScore+TScore+RScore+CScore)

gcard20<-read_csv(here::here("data","gcarddata2.csv"))

gcard21<-rbind(gcard20,gcard19)

write_csv(x = gcard21,file = here::here("data","gcarddata2.csv"),append = FALSE)

gcard22<-gcard21 %>%
  group_by(`Boar Stud`) %>%
  top_n(5,yrwk) %>% 
  filter(yrwk!=max(yrwk)) %>% 
  summarise(`4 Week Average`=mean(Total))

gcard23<-gcard21 %>% 
  group_by(`Boar Stud`) %>% 
  top_n(13,yrwk) %>% 
  summarise(`Quarter Average`=mean(Total))

gcard24<-left_join(x = gcard19,y = gcard22,by=c("Boar Stud"="Boar Stud"))
gcard25<-left_join(x = gcard24,y = gcard23,by=c("Boar Stud"="Boar Stud"))

gcard25<-gcard25[c(1,7,19,14,20,10,21,18,22,23,24,25)]

write_csv(x = gcard25,path = here::here("data","gcard2.csv"),append = FALSE)
