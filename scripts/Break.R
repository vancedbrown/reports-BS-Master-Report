library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

pigraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/pig.csv", 
                 col_types = cols(Index = col_number()))
collraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")
distraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/dist.csv")
splitraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/split.csv")

pigraw$Date_Arrival<-as.Date(pigraw$Date_Arrival)
pigraw$Date_Studout<-as.Date(pigraw$Date_Studout)
collraw$Col_Date<-as.Date(collraw$Col_Date)
distraw$Date_Shipped<-as.Date(distraw$Date_Shipped)
splitraw$Collnum<-as.numeric(splitraw$Collnum)

break1<-pigraw %>% 
  filter(Date_Arrival<=floor_date(x = today(), unit = "week", week_start = 7)-7) %>% 
  filter(is.na(Date_Studout)|Date_Studout>floor_date(x = today(),unit = "week", week_start = 7))

break2<-collraw %>% 
  filter(Col_Date>floor_date(x = today(),unit = "week", week_start = 7)-7) %>% 
  filter(Col_Date<=floor_date(x = today(), unit = "week", week_start = 7))

break3<-full_join(x = distraw, y = splitraw, by=c("BatchNum"="BatchNum", "Boar Stud"="Boar Stud"))

break4<-break3 %>%
  filter(Dest=='* TRASH *')

break5<-break3 %>%
  filter(Dest!='* TRASH *')

break6<-anti_join(x = break5,y = break4, by=c("BoarID"="BoarID", "Collnum"="Collnum"))

break7<-left_join(x = break2, y = break5, by=c("BoarID"="BoarID", "Collnum"="Collnum"))

break8<-break7[!duplicated(break7[c("Boar Stud.x","Collnum")]), ]

break9<-left_join(x = break1, y = break8, by=c("BoarID"="BoarID"))

break10<-break9 %>% 
  mutate('Collected'=ifelse(`Collection Status`%in%c('US','TR'),'YES', 'NO'),
         'Distributed'=ifelse(is.na(Dest),'NO','YES'))

break11<-break10[c(1:4,10,11,33,17,34,8)]

break11<-break11[order(break11$`Boar Stud`,break11$Breed.x,-break11$Index),]

write_csv(x = break11,path = here::here("data","breakdown.csv"),append = FALSE)

#########################################################################

break11<-break11[order(break11$BoarID,break11$`Collection Status`, break11$Distributed),]

break12<-break11 %>%
  group_by(BoarID) %>% 
  mutate(dr=row_number(Distributed),
         count=rowsum(dr,group = BoarID))

break13<-break12 %>% 
  group_by(BoarID) %>% 
  mutate('Included'=ifelse(`Boar Status`=='NONWORKING', 'NO',
                           ifelse(dr==max(dr) | is.na(dr),'YES','NO')))

break14<-break13 %>% 
  filter(Included=='YES',
         Distributed=='YES' | `Collection Status`=='TR')

break14<-break14[order(break14$`Boar Stud`,break14$Breed.x,-break14$Index),]

break15<-break14 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate(rank=row_number(-Index),
         maxrank=ifelse(length(Distributed[Distributed=='YES'])==0,0,max(rank[Distributed=='YES'])))

break16<-break15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  summarise(actual=ifelse(mean(maxrank)==0,0,mean(Index[rank<=maxrank])))

break17<-break15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  summarize('Needed'=mean(maxrank))

break18<-break13 %>% 
  filter(Included=='YES')

break19<-left_join(x = break18,y = break17,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))

break20<-break19 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate(rank=row_number(-Index)) %>% 
  summarise(target=ifelse(mean(Needed)==0,0,mean(Index[rank<=Needed])))

break21<-left_join(x = break16,y = break20,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))

break21$`Missed Index`<-break21$target-break21$actual

break22<-break19 %>%
  group_by(`Boar Stud`) %>% 
  mutate(wc=floor_date(Col_Date,unit = "week",week_start = 1)) %>%  
  filter(!is.na(wc)) %>% 
  summarise(WeekCommencing=max(wc))

break23<-left_join(x = break21,y = break22,by=c("Boar Stud"="Boar Stud"))

break24<-break23 %>% 
  filter(Breed.x=='SPG240')

break24<-break24[c(-2)]

write_csv(x = break24,path = here::here("data","topn.csv"), append = FALSE)
write_csv(x = break23,path = here::here("data","missedindexall.csv"), append = TRUE, col_names = TRUE)


################
break25<-break15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate("Actual"=ifelse(rank<=maxrank,1,0))

break26<-break19 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate(rank=row_number(-Index)) %>% 
  mutate('Target'=ifelse(rank<=Needed,1,0))


break27<-left_join(x = break13,y = break17,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))
break28<-left_join(x = break27,y = break25,by=c("Boar Stud"="Boar Stud","Collnum"="Collnum"))

break29<-break28 %>% 
  filter(!`Boar Stud`=='High Desert')

break29<-break29[c(1:4,6:10,13,14,28)]

break29<-break29[order(break29$Breed.x.x,-break29$Index.x),]

write_csv(x = break29,path = here::here("data","break.csv"), append = FALSE)

breakhigh<-read_csv(here::here("data","hdmi.csv"))

break29a<-rbind(break29,breakhigh)

high<-break29a %>% filter(`Boar Stud`=='High Desert')
spgn<-break29a %>% filter(`Boar Stud`=='SPGNC')
spgv<-break29a %>% filter(`Boar Stud`=='SPGVA')
spgt<-break29a %>% filter(`Boar Stud`=='SPGTX')
deer<-break29a %>% filter(`Boar Stud`=='MB 7081')
laur<-break29a %>% filter(`Boar Stud`=='MB 7082')
aski<-break29a %>% filter(`Boar Stud`=='MB 7092')
will<-break29a %>% filter(`Boar Stud`=='MB 7093')
robe<-break29a %>% filter(`Boar Stud`=='MB 7094')
cima<-break29a %>% filter(`Boar Stud`=='MBW Cimarron')
cycl<-break29a %>% filter(`Boar Stud`=='MBW Cyclone')
illi<-break29a %>% filter(`Boar Stud`=='MBW Illinois')
prin<-break29a %>% filter(`Boar Stud`=='Princeton')
skyl<-break29a %>% filter(`Boar Stud`=='Skyline Boar Stud')
yuma<-break29a %>% filter(`Boar Stud`=='MBW Yuma')
cian<-break29a %>% filter(`Boar Stud`=='Norson')
gcm<-break29a %>% filter(`Boar Stud`=='GCM7-4')

write_xlsx(x=list("High Desert"=high,
                  "SPG NC"=spgn,
                  "SPG VA"=spgv,
                  "SPG TX"=spgt,
                  "Deercroft"=deer,
                  "Laurel Hill"=laur,
                  "Askin"=aski,
                  "Williamston"=will,
                  "Robersonville"=robe,
                  "Cimarron"=cima,
                  "Cyclone"=cycl,
                  "Illinois"=illi,
                  "Princeton"=prin,
                  "Skyline"=skyl,
                  "Yuma"=yuma,
                  "Norson"=cian,
                  "GCM"=gcm),
           here::here("data","MissedIndex_Breakdown.xlsx"))

################# Weighted by line#################

# break30<-left_join(x = break23,y = break17, by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))
# 
# break31<-break30 %>%
#   group_by(`Boar Stud`) %>%
#   filter(Breed.x%in%c('SPG120','SPG240')) %>%
#   mutate(totalneeded=sum(Needed))
# 
# break31$missed_partial<-break31$`Missed Index`*(break31$Needed/break31$totalneeded)
# 
# break32<-break31 %>%
#   group_by(`Boar Stud`) %>%
#   mutate(missed=sum(missed_partial)) %>%
#   filter(Breed.x==ifelse(`Boar Stud`=='MBW Illinois','SPG120','SPG240'))
# 
# break32<-break32[c(1,3,4,10,6)]
# 
# break32$`Missed Index`<-break32$missed
# 
# break33<-break32[c(1,2,3,6,5)]
# 
# write_csv(x = break33,path = here::here("data","topn.csv"), append = FALSE)
