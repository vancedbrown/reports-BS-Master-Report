library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

collraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/coll.csv",
                  col_types = cols(Ext_Batch_Num = col_character()))


water1<-collraw %>% 
  mutate('Num'=as.character(Ext_Batch_Num))

reading1<-function(my.string){unlist(strsplit(my.string,"_"))[1]}

reading2<-function(my.string){unlist(strsplit(my.string," "))[1]}

reading3<-function(my.string){unlist(strsplit(my.string,"/"))[1]}

water1$read1<-sapply(water1$Num,reading1)

water1$read1<-as.numeric(water1$read1)

water1$read2<-sapply(water1$Num,reading2)

water1$read2<-as.numeric(water1$read2)

water1$read3<-sapply(water1$Num,reading3)

water1$read3<-as.numeric(water1$read3)

water1<-water1[c(1,4,19:21)]

water1[is.na(water1)]<-0

water1$read4<-with(water1,pmax(read1,read2,read3))

water2<-water1 %>% 
  mutate(read5=ifelse(read4<20,read4,
                      ifelse(read4==100,'0',
                             ifelse(read4%in%c(20:100),read4/10,read4/100))))

water2$read5<-as.numeric(water2$read5)


water3<-water2 %>% 
  filter(Col_Date>=floor_date(x = today(),unit = "week",week_start = 1)-7,
         Col_Date<floor_date(x = today(),unit = "week",week_start = 1))

water4<-water3 %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Possible Readings'=n_distinct(Col_Date))

water5<-water3 %>% 
  filter(Col_Date==max(Col_Date)-6,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Monday'=mean(read5))

water6<-water3 %>% 
  filter(Col_Date==max(Col_Date)-5,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Tuesday'=mean(read5))

water7<-water3 %>% 
  filter(Col_Date==max(Col_Date)-4,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Wednesday'=mean(read5))

water8<-water3 %>% 
  filter(Col_Date==max(Col_Date)-3,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Thursday'=mean(read5))

water9<-water3 %>% 
  filter(Col_Date==max(Col_Date)-2,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Friday'=mean(read5))

water10<-water3 %>% 
  filter(Col_Date==max(Col_Date)-1,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Saturday'=mean(read5))

water11<-water3 %>% 
  filter(Col_Date==max(Col_Date),
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Sunday'=mean(read5))

water12<-left_join(x = water4,y = water5,by=c("Boar Stud"="Boar Stud"))
water13<-left_join(x = water12,y = water6,by=c("Boar Stud"="Boar Stud"))
water14<-left_join(x = water13,y = water7,by=c("Boar Stud"="Boar Stud"))
water15<-left_join(x = water14,y = water8,by=c("Boar Stud"="Boar Stud"))
water16<-left_join(x = water15,y = water9,by=c("Boar Stud"="Boar Stud"))
water17<-left_join(x = water16,y = water10,by=c("Boar Stud"="Boar Stud"))
water18<-left_join(x = water17,y = water11,by=c("Boar Stud"="Boar Stud"))

water19<-water18 %>% 
  filter(`Boar Stud`%in%c('High Desert',
                          'MB 7081',
                          'MB 7082',
                          'MB 7092',
                          'MB 7093',
                          'MB 7094',
                          'MBW Cimarron',
                          'MBW Cyclone',
                          'MBW Illinois',
                          'MBW Yuma',
                          'Princeton',
                          'Skyline Boar Stud',
                          'SPGNC',
                          'SPGVA',
                          'SPGTX'))

write_csv(x = water19, path = here::here("data","water.csv"),append = FALSE)
