library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv")
collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")

pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)
collraw$Col_Date<-as.Date(collraw$Col_Date)


fday1<-pigraw %>% 
  filter(is.na(Date_Studout))

fday2<-collraw %>% 
  group_by(BoarID) %>% 
  #filter(Status!='NC') %>% 
  top_n(1,Col_Date)

fday3<-collraw %>% 
  group_by(`Boar Stud`) %>% 
  summarise(date=max(Col_Date))

fday4<-left_join(x = fday1,y = fday2, by=c("BoarID"="BoarID"))

fday5<-left_join(x = fday4,y = fday3,by=c("Boar Stud.x"="Boar Stud"))

fday5$rest<-fday5$date-fday5$Col_Date
fday5$stay<-fday5$date-fday5$Date_Arrival

fday6<-fday5 %>% 
  filter(stay>15) %>% 
  filter(rest>15|is.na(rest))

fday7<-fday6 %>% 
  group_by(`Boar Stud.x`) %>% 
  summarize('14 Day Rest'=n_distinct(BoarID))

write_csv(x = fday6,path = here::here("data","14daypigs.csv"),append = FALSE)
write_csv(x = fday7,path = here::here("data","14day.csv"),append = FALSE)

