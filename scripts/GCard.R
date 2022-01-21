library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(here)

gcard000<-read_csv(here::here("data","avginv.csv"))
gcard00<-read_csv(here::here("data","topn_high.csv"))
gcard0<-read_csv(here::here("data","topn_nuc.csv"))
gcard1<- read_csv(here::here("data","inventory.csv"))
gcard2<-read_csv(here::here("data","topn.csv"))
gcard3<-read_csv(here::here("data","rest.csv"))
gcard4<-read_csv(here::here("data","cull.csv"))
gcard5<-read_csv(here::here("data","breakdown.csv"))
gcard6<-read_csv(here::here("data","trained.csv"))

gcard2$WeekCommencing<-as.Date(gcard2$WeekCommencing)
gcard4$WeekCommencing<-as.Date(gcard4$WeekCommencing, format="%m/%d/%Y")
gcard2$yrwk<-(year((gcard2$WeekCommencing)+5)*100)+isoweek((gcard2$WeekCommencing)+5)

gcard0$WeekCommencing<-as.Date(gcard0$WeekCommencing)
gcard0$yrwk<-(year((gcard0$WeekCommencing)+5)*100)+isoweek((gcard0$WeekCommencing)+5)

gcard00$WeekCommencing<-as.Date(gcard00$WeekCommencing)
gcard00$yrwk<-(year((gcard00$WeekCommencing)+5)*100)+isoweek((gcard00$WeekCommencing)+5)

gcard2a<-gcard2 %>% 
  filter(!`Boar Stud`%in%c('SPGNC','SPGVA','SPGTX','High Desert'))

gcard2b<-rbind(gcard2a,gcard0,gcard00)

gcard7<-gcard5 %>% 
  group_by(`Boar Stud`) %>% 
  filter(Breed.x==ifelse(`Boar Stud`=='MBW Illinois','SPG120','SPG240')) %>% 
  summarise(boars=n_distinct(BoarID))

gcard8<-gcard5 %>% 
  group_by(`Boar Stud`) %>% 
  filter(Breed.x==ifelse(`Boar Stud`=='MBW Illinois','SPG120','SPG240'),
         Distributed=='YES' | `Collection Status`=='TR') %>% 
  summarise(dist=n_distinct(BoarID))

gcard4a<-gcard4 %>% 
  filter(WeekCommencing>=floor_date(x = today(),unit = "week",week_start = 7)-161) %>% 
  group_by(`Boar Stud`) %>% 
  summarise(`% Boars Culled in Bottom 25%`=(sum(`Low Index Culls`)/sum(`Boars Culled`))*100)

gcard4b<-gcard4 %>% 
  filter(WeekCommencing>=floor_date(x = today(),unit = "week",week_start = 7)-161) %>% 
  group_by(`Boar Stud`) %>% 
  summarise(`Total Boars Culled`=sum(`Boars Culled`))

gcard4c<-left_join(x = gcard4b,y = gcard4a,by=c("Boar Stud"="Boar Stud"))

gcard9<-left_join(x = gcard1,y = gcard7,by=c("Boar Stud"="Boar Stud"))
gcard10<-left_join(x = gcard9,y = gcard8,by=c("Boar Stud"="Boar Stud"))
gcard11<-left_join(x = gcard10,y = gcard2b,by=c("Boar Stud"="Boar Stud"))
gcard12<-left_join(x = gcard11,y = gcard3, by=c("Boar Stud"="Boar Stud"))
gcard13<-left_join(x = gcard12,y = gcard4c,by=c("Boar Stud"="Boar Stud"))
gcard14<-left_join(x = gcard13,y = gcard6,by=c("Boar Stud"="Boar Stud"))

gcard14$`% Boars Distributed`<-(gcard14$dist/gcard14$boars)*100

gcard15<-gcard14 %>% 
  filter(!is.na(WeekCommencing))

gcard15[is.na(gcard15)]<-0

gcard15a<-left_join(x = gcard15,y = gcard000,by=c("Boar Stud"="Boar Stud"))

gcard15a$rate<-gcard15a$`Total Boars Culled`/gcard15a$`Average Inventory`

gcard16<-gcard15a %>% 
  mutate(IScore=ifelse(`Total Boars Culled`<=5,3,
                       ifelse(as.integer(`% Boars Culled in Bottom 25%`)>=50,3,
                       ifelse(as.integer(`% Boars Culled in Bottom 25%`)>=40,2,
                              ifelse(as.integer(`% Boars Culled in Bottom 25%`)>=30,1,0)))),
         TScore=ifelse(`Missed Index`<=0.25,3,
                              ifelse(`Missed Index`<=0.50,2,
                                     ifelse(`Missed Index`<=0.75,1,0))),
         DScore=ifelse(abs(`Average Days Rest`-7)<=1,3,
                       ifelse(abs(`Average Days Rest`-7)<=2,2,
                              ifelse(abs(`Average Days Rest`-7)<=3,1,0))),
         BScore=ifelse(`Total Boars`==0,3,
                       ifelse(`Percent Trained`==0,3,
                              ifelse(`Percent Trained`>=90,3,
                                     ifelse(`Percent Trained`>=80,2,
                                            ifelse(`Percent Trained`>=70,1,0))))),
         Total=IScore+TScore+DScore+BScore)

gcard16$wk<-isoweek(gcard16$WeekCommencing)
gcard16$qt<-ifelse(gcard16$wk%in%c(1:13),1,
                  ifelse(gcard16$wk%in%c(14:26),2,
                         ifelse(gcard16$wk%in%c(27:39),3,4)))

gcard16$yr<-year((gcard16$WeekCommencing+5))

#write_csv(x = gcard16,path = here::here("data","gcarddata.csv"))

gcard17<-read_csv(here::here("data","gcarddata.csv"))
gcard18<-read_csv(here::here("inputs","months.csv"))
gcard18$Week<-as.numeric(gcard18$Week)

gcard17<-gcard17[-c(30)]

gcard19<-rbind(gcard17,gcard16)

gcard20<-left_join(x = gcard19,y = gcard18, by=c("yrwk"="Week"))

write_csv(x = gcard20,path = here::here("data","gcarddata.csv"),append = FALSE)

gcard21<-gcard19 %>%
  group_by(`Boar Stud`) %>%
  top_n(5,yrwk) %>% 
  filter(yrwk!=max(yrwk)) %>% 
  summarise(`4 Week Average`=mean(Total))

gcard22<-gcard19 %>% 
  group_by(`Boar Stud`) %>% 
  top_n(13,yrwk) %>% 
  summarise(`Quarter Average`=mean(Total))

gcard23<-left_join(x = gcard16,y = gcard21,by=c("Boar Stud"="Boar Stud"))
gcard24<-left_join(x = gcard23,y = gcard22,by=c("Boar Stud"="Boar Stud"))

gcard24<-gcard24[c(1,2,19,9,23,12,24,18,25,15,22,26,30,31)]

#gcard22<-gcard21 %>% 
  #filter(`Boar Stud`!='Prestage MS')

write_csv(x = gcard24,path = here::here("data","gcard.csv"),append = FALSE)

