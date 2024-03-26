library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(here)

dash1<-read_csv(here::here("data","scard2.csv"))
dash2<-read_csv(here::here("data","pcard2.csv"))
dash3<-read_csv(here::here("data","gcard2.csv"))


dash4<-dash1 %>% 
  mutate(`Semen QCQA Score`=as.numeric(Total),
         `13 Week Semen QCQA Score`=as.numeric(`13 Week Average`))

dash4<-dash4[c(1,14,15)]

dash5<-dash2 %>% 
  mutate(`Production Score`=Total,
         `13 Week Production Score`=`Quarter Average`)

dash5<-dash5[c(1,13,14)]

dash6<-dash3 %>% 
  mutate(`Genetic Implementation Score`=Total,
         `13 Week Genetic Implementation Score`=`Quarter Average`)

dash6<-dash6[c(1,13,14)]

dash7<-left_join(x = dash4,y = dash5,by=c("Boar Stud"="Boar Stud"))
dash8<-left_join(x = dash7,y = dash6,by=c("Boar Stud"="Boar Stud"))

dash8$`Total Score`<-dash8$`Semen QCQA Score`+dash8$`Production Score`+dash8$`Genetic Implementation Score`
dash8$`13 Week Total Score`<-dash8$`13 Week Semen QCQA Score`+dash8$`13 Week Production Score`+dash8$`13 Week Genetic Implementation Score`

dash9<-dash8 %>% 
  filter(!`Boar Stud`%in%c('SPG Farm 62','SPG9644'))

write_csv(dash9,path = here::here("data","dash2.csv"))

