library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv",
                  col_types = cols(Ext_Batch_Num = col_character()))


cond1<-collraw %>% 
  mutate('Num'=as.character(Ext_Batch_Num))

reading1<-function(my.string){unlist(strsplit(my.string,"_"))[3]}

reading2<-function(my.string){unlist(strsplit(my.string," "))[3]}

reading3<-function(my.string){unlist(strsplit(my.string,"/"))[3]}

cond1$read1<-sapply(cond1$Num,reading1)

cond1$read1<-as.numeric(cond1$read1)

cond1$read2<-sapply(cond1$Num,reading2)

cond1$read2<-as.numeric(cond1$read2)

cond1$read3<-sapply(cond1$Num,reading3)

cond1$read3<-as.numeric(cond1$read3)

cond1<-cond1[c(1,4,19:21)]

cond1[is.na(cond1)]<-0

cond1$read4<-with(cond1,pmax(read1,read2,read3))

cond2<-cond1 %>% 
  mutate(read5=ifelse(read4<20,read4,
                      ifelse(read4==100,'0',
                             ifelse(read4%in%c(20:100),read4/10,read4/100))))

cond2$read5<-as.numeric(cond2$read5)


cond3<-cond2 %>% 
  filter(Col_Date>=floor_date(x = today(),unit = "week",week_start = 1)-7,
         Col_Date<floor_date(x = today(),unit = "week",week_start = 1))

cond4<-cond3 %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Possible Readings'=n_distinct(Col_Date))

cond5<-cond3 %>% 
  filter(Col_Date==max(Col_Date)-6,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Monday'=mean(read5))

cond6<-cond3 %>% 
  filter(Col_Date==max(Col_Date)-5,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Tuesday'=mean(read5))

cond7<-cond3 %>% 
  filter(Col_Date==max(Col_Date)-4,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Wednesday'=mean(read5))

cond8<-cond3 %>% 
  filter(Col_Date==max(Col_Date)-3,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Thursday'=mean(read5))

cond9<-cond3 %>% 
  filter(Col_Date==max(Col_Date)-2,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Friday'=mean(read5))

cond10<-cond3 %>% 
  filter(Col_Date==max(Col_Date)-1,
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Saturday'=mean(read5))

cond11<-cond3 %>% 
  filter(Col_Date==max(Col_Date),
         read5>0) %>% 
  group_by(`Boar Stud`) %>% 
  summarise('Sunday'=mean(read5))

cond12<-left_join(x = cond4,y = cond5,by=c("Boar Stud"="Boar Stud"))
cond13<-left_join(x = cond12,y = cond6,by=c("Boar Stud"="Boar Stud"))
cond14<-left_join(x = cond13,y = cond7,by=c("Boar Stud"="Boar Stud"))
cond15<-left_join(x = cond14,y = cond8,by=c("Boar Stud"="Boar Stud"))
cond16<-left_join(x = cond15,y = cond9,by=c("Boar Stud"="Boar Stud"))
cond17<-left_join(x = cond16,y = cond10,by=c("Boar Stud"="Boar Stud"))
cond18<-left_join(x = cond17,y = cond11,by=c("Boar Stud"="Boar Stud"))

cond19<-cond18 %>% 
  filter(`Boar Stud`%in%c('High Desert',
                          'MB 7081',
                          'MB 7082',
                          'MB 7092',
                          'MB 7093',
                          'MB 7094',
                          'MBW Cimarron',
                          'MBW Cyclone',
                          'MBW Yuma',
                          'Skyline Boar Stud',
                          'SPGNC',
                          'SPGVA',
                          'SPG9644'))

write_csv(x = cond19, path = here::here("data","cond.csv"),append = FALSE)
