library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv")
collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")

pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)
collraw$Col_Date<-as.Date(collraw$Col_Date)

collraw$wc<-floor_date(x = collraw$Col_Date,unit = "week",week_start = 7)

collraw<-collraw[order(collraw$BoarID,collraw$Col_Date),]

rest1<-collraw %>%
  filter(wc<floor_date(x = today(),unit = "week",week_start = 7)) %>% 
  filter(`Collection Status`!='NC') %>% 
  group_by(BoarID, `Boar Stud`) %>% 
  mutate(dr=row_number(desc(Col_Date)),
         count=rowsum(dr,group = BoarID))

rest2<-rest1 %>% 
  group_by(BoarID) %>% 
  filter(count!=1)

rest3<-rest2 %>% 
  group_by(BoarID, `Boar Stud`) %>% 
  mutate(date1=Col_Date[dr==1],
         date2=Col_Date[dr==2],
         rest=date1-date2) %>% 
  filter(rest!=0)

rest4<-rest3 %>% 
  group_by(BoarID) %>% 
  top_n(1,Col_Date)

rest5<-rest4 %>% 
  filter(wc==floor_date(x = today(),unit = "week",week_start = 7)-7,
         BoarID!='40005')

write_csv(rest5, path = here::here("data","fgh.csv"))

rest6<-rest5 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Average Days Rest'=mean(rest))

rest7<-rest5 %>%
  group_by(`Boar Stud`) %>% 
  summarize('Standard Deviation'=sd(rest))

rest8<-left_join(x = rest6,y = rest7,by=c("Boar Stud"="Boar Stud"))

# rest9a<-rest1 %>%
#   group_by(BoarID) %>% 
#   filter(`Boar Stud`!='SPGNC',
#          count>10)
# 
# rest9b<-rest1 %>% 
#   group_by(BoarID) %>% 
#   filter(`Boar Stud`=='SPGNC')

# rest9<-rbind(rest9a, rest9b)

rest9<-left_join(x = rest1, y = pigraw, by=c("BoarID"="BoarID"))

rest10<-rest9 %>%
  filter(wc==floor_date(x = today(),unit = "week",week_start = 7)-7) %>% 
  filter(`Collection Status`=='TR',
         `Boar Status`=='WORKING') %>% 
  group_by(`Boar Stud.x`) %>% 
  summarize(trash=n())

rest11<-rest9 %>% 
  filter(wc==floor_date(x = today(),unit = "week",week_start = 7)-7) %>% 
  filter(`Collection Status`!='NC',
         `Boar Status`=='WORKING') %>%
  group_by(`Boar Stud.x`) %>% 
  summarize(total=n())

rest12<-left_join(x = rest10,y = rest11,by=c("Boar Stud.x"="Boar Stud.x"))
rest12$`Trash Rate`<-(rest12$trash/rest12$total)*100

rest12$`Boar Stud`<-rest12$`Boar Stud.x`
rest12<-rest12[c(5,2,3,4)]

write_csv(x = rest8,here::here("data","rest.csv"),append = FALSE)
write_csv(x = rest12,here::here("data","trash.csv"),append = FALSE)
