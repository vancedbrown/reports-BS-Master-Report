library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(here)

dash1<-read_csv(here::here("data","scard.csv"))
dash2<-read_csv(here::here("data","pcard.csv"))
dash3<-read_csv(here::here("data","gcard.csv"))
dash4<-read_csv(here::here("inputs","match.csv"))


dash5<-dash1 %>% 
  mutate(`Semen QCQA Score`=Comb_score,
         `13 Week Semen QCQA Score`=q)

dash6<-left_join(x = dash4,y = dash5,by=c("Stud"="StudName"))
dash6<-dash6[c(1,19,20)]

dash7<-dash2 %>% 
  mutate(`Production Score`=Total,
         `13 Week Production Score`=`Quarter Average`)
dash7<-dash7[c(1,15,16)]

dash8<-dash3 %>% 
  mutate(`Genetic Implementation Score`=Total,
         `13 Week Genetic Implementation Score`=`Quarter Average`)
dash8<-dash8[c(1,15,16)]

dash9<-left_join(x = dash6,y = dash7,by=c("Boar Stud"="Boar Stud"))
dash10<-left_join(x = dash9,y = dash8,by=c("Boar Stud"="Boar Stud"))
dash10$`Total Score`<-dash10$`Semen QCQA Score`+dash10$`Production Score`+dash10$`Genetic Implementation Score`
dash10$`13 Week Total Score`<-dash10$`13 Week Semen QCQA Score`+dash10$`13 Week Production Score`+dash10$`13 Week Genetic Implementation Score`

dash11<-dash10 %>% 
  filter(!`Boar Stud`%in%c('MBW Illinois', 'SPGTX'))

write_csv(dash11,path = here::here("data","dash.csv"))

