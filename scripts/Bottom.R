library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/pig.csv", 
                 col_types = cols(Index = col_number()))

pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)

bottom1<-pigraw %>% 
  filter(`Boar Status`%in%c('WORKING','NONWORKING')) %>% 
  group_by(Breed) %>% 
  filter(!is.na(Index)) %>% 
  mutate(rank=row_number(Index),
         rankp=rank/max(rank))

bottom2<-bottom1 %>% 
  filter(rankp<=0.100)

write_csv(x = bottom2,path = here::here("data","bottom.csv"),append = FALSE)

