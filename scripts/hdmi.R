library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/pig.csv", 
                 col_types = cols(Index = col_number()))
collraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/coll.csv")
distraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/dist.csv")
splitraw<-read_csv("C:/Users/vance/Documents/projects/2019/01JAN/Data Pull/split.csv")

pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)
collraw$Col_Date<-as.Date(collraw$Col_Date)
distraw$Date_Shipped<-as.Date(distraw$Date_Shipped)
splitraw$Collnum<-as.numeric(splitraw$Collnum)

hdmi1<-pigraw %>% 
  filter(Date_Arrival<=floor_date(x = today(), unit = "week", week_start = 7)-8) %>% 
  filter(is.na(Date_Studout)|Date_Studout>floor_date(x = today(),unit = "week", week_start = 7))

hdmi2<-collraw %>% 
  filter(Col_Date>floor_date(x = today(),unit = "week", week_start = 7)-8) %>% 
  filter(Col_Date<=floor_date(x = today(), unit = "week", week_start = 7))

hdmi3<-full_join(x = distraw, y = splitraw, by=c("BatchNum"="BatchNum", "Boar Stud"="Boar Stud"))

hdmi4<-hdmi3 %>%
  filter(Dest=='* TRASH *')

hdmi5<-hdmi3 %>%
  filter(Dest!='* TRASH *')

hdmi6<-anti_join(x = hdmi5,y = hdmi4, by=c("BoarID"="BoarID", "Collnum"="Collnum"))

hdmi7<-left_join(x = hdmi2, y = hdmi5, by=c("BoarID"="BoarID", "Collnum"="Collnum"))

hdmi8<-hdmi7[!duplicated(hdmi7[c("Boar Stud.x","Collnum")]), ]

hdmi9<-left_join(x = hdmi1, y = hdmi8, by=c("BoarID"="BoarID"))

hdmi10<-hdmi9 %>% 
  filter(`Boar Stud.x`=='High Desert') %>% 
  mutate('Collected'=ifelse(`Collection Status`%in%c('US','TR'),'YES', 'NO'),
         'Distributed'=ifelse(is.na(Dest),'NO','YES'))

hdmi11<-hdmi10[c(1:4,10,11,33,17,34,8)]

hdmi11<-hdmi11[order(hdmi11$`Boar Stud`,hdmi11$Breed.x,-hdmi11$Index),]

write_csv(x = hdmi11,path = here::here("data","hdmidown.csv"),append = FALSE)

#########################################################################

hdmi11<-hdmi11[order(hdmi11$BoarID,hdmi11$`Collection Status`, hdmi11$Distributed),]

hdmi12<-hdmi11 %>%
  group_by(BoarID) %>% 
  mutate(dr=row_number(`Collection Status`),
         count=rowsum(dr,group = BoarID))

hdmi13<-hdmi12 %>% 
  group_by(BoarID) %>% 
  mutate('Included'=ifelse(`Boar Status`=='NONWORKING', 'NO',
                           ifelse(dr==max(dr) | is.na(dr),'YES','NO')))

hdmi14<-hdmi13 %>% 
  filter(Included=='YES',
         Distributed=='YES' | `Collection Status`=='TR')

hdmi14<-hdmi14[order(hdmi14$`Boar Stud`,hdmi14$Breed.x,-hdmi14$Index),]

hdmi15<-hdmi14 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate(rank=row_number(-Index),
         maxrank=ifelse(length(Distributed[Distributed=='YES'])==0,0,max(rank[Distributed=='YES'])))

hdmi16<-hdmi15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  summarise(actual=ifelse(mean(maxrank)==0,0,mean(Index[rank<=maxrank])))

hdmi17<-hdmi15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  summarize('Needed'=mean(maxrank))

hdmi18<-hdmi13 %>% 
  filter(Included=='YES')

hdmi19<-left_join(x = hdmi18,y = hdmi17,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))

hdmi20<-hdmi19 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate(rank=row_number(-Index)) %>% 
  summarise(target=ifelse(mean(Needed)==0,0,mean(Index[rank<=Needed])))

hdmi21<-left_join(x = hdmi16,y = hdmi20,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))

hdmi21$`Missed Index`<-hdmi21$target-hdmi21$actual

hdmi22<-hdmi19 %>%
  group_by(`Boar Stud`) %>% 
  mutate(wc=floor_date(Col_Date,unit = "week",week_start = 1)-7) %>%  
  filter(!is.na(wc)) %>% 
  summarise(WeekCommencing=max(wc))

hdmi23<-left_join(x = hdmi21,y = hdmi22,by=c("Boar Stud"="Boar Stud"))

hdmi24<-hdmi23 %>% 
  filter(Breed.x=='SPG240')

hdmi24<-hdmi24[c(-2)]

write_csv(x = hdmi24,path = here::here("data","topn_high.csv"), append = FALSE)
write_csv(x = hdmi23,path = here::here("data","missedindexhigh.csv"), append = TRUE, col_names = TRUE)


################
hdmi25<-hdmi15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate("Actual"=ifelse(rank<=maxrank,1,0))

hdmi26<-hdmi19 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate(rank=row_number(-Index)) %>% 
  mutate('Target'=ifelse(rank<=Needed,1,0))


hdmi27<-left_join(x = hdmi13,y = hdmi17,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))
hdmi28<-left_join(x = hdmi27,y = hdmi25,by=c("Boar Stud"="Boar Stud","Collnum"="Collnum"))

hdmi29<-hdmi28

hdmi29<-hdmi29[c(1:4,6:10,13,14,28)]

hdmi29<-hdmi29[order(hdmi29$Breed.x.x,-hdmi29$Index.x),]

write_csv(x = hdmi29,path = here::here("data","hdmi.csv"), append = FALSE)


################# Weighted by line#################

# hdmi30<-left_join(x = hdmi23,y = hdmi17, by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))
# 
# hdmi31<-hdmi30 %>%
#   group_by(`Boar Stud`) %>%
#   filter(Breed.x%in%c('SPG120','SPG240')) %>%
#   mutate(totalneeded=sum(Needed))
# 
# hdmi31$missed_partial<-hdmi31$`Missed Index`*(hdmi31$Needed/hdmi31$totalneeded)
# 
# hdmi32<-hdmi31 %>%
#   group_by(`Boar Stud`) %>%
#   mutate(missed=sum(missed_partial)) %>%
#   filter(Breed.x==ifelse(`Boar Stud`=='MBW Illinois','SPG120','SPG240'))
# 
# hdmi32<-hdmi32[c(1,3,4,10,6)]
# 
# hdmi32$`Missed Index`<-hdmi32$missed
# 
# hdmi33<-hdmi32[c(1,2,3,6,5)]
# 
# write_csv(x = hdmi33,path = here::here("data","topn_high.csv"), append = FALSE)