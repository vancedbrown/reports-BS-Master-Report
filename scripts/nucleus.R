
library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(writexl)
library(here)

library(readr)

studs<-c('SPGNC',
         'SPGVA',
         'SPGTX',
         'MB 7092')

source('C:/Users/vance/Documents/myR/functions/getSQL.r')

all <- "SELECT [StudID] as 'Boar Stud'
,[Tattoo]
,[Target]
,[SelDate]
,[UnSelDate]
FROM [Intranet].[dbo].[Boar_NonPooled]"
alloc <- getSQL('Intranet',query=all)

alloc$SelDate<-as.Date(alloc$SelDate)
alloc$UnSelDate<-as.Date(alloc$UnSelDate)

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

nuc1<-pigraw %>% 
  filter(`Boar Stud`%in%studs) %>% 
  filter(Date_Arrival<=floor_date(x = today(), unit = "week", week_start = 7)-7) %>% 
  filter(is.na(Date_Studout)|Date_Studout>floor_date(x = today(),unit = "week", week_start = 7))

nuc2<-collraw %>% 
  filter(`Boar Stud`%in%studs) %>% 
  filter(Col_Date>floor_date(x = today(),unit = "week", week_start = 7)-7) %>% 
  filter(Col_Date<=floor_date(x = today(), unit = "week", week_start = 7))

nuc3<-full_join(x = distraw, y = splitraw, by=c("BatchNum"="BatchNum", "Boar Stud"="Boar Stud"))

nuc4<-nuc3 %>%
  filter(`Boar Stud`%in%studs) %>% 
  filter(Dest=='* TRASH *')

nuc5<-nuc3 %>%
  filter(`Boar Stud`%in%studs) %>% 
  filter(Dest!='* TRASH *')

nuc6<-anti_join(x = nuc5,y = nuc4, by=c("BoarID"="BoarID", "Collnum"="Collnum"))

nuc7<-left_join(x = nuc2, y = nuc5, by=c("BoarID"="BoarID", "Collnum"="Collnum"))

nuc9<-left_join(x = nuc1, y = nuc7, by=c("BoarID"="BoarID"))

nuc9a<-nuc9 %>% 
  filter(Dest%in%c('103307- WOLF 7 TRAD',
                   '27041',
                   '27041 TRAD',
                   '71921 TRAD',
                   'SPG FARM 60 TRAD',
                   'SPG FARM 61 TRAD',
                   'SPG FARM 62 TRAD',
                   'SPG FARM 63 TRAD',
                   'SPG FARM 64 TRAD',
                   'SPG Farm 62',
                   'TA WOMBLE TRAD',
                   'WOLF 7  (PSFMO) TRAD',
                   'GREEN MEADOWS SOW PCAI',
                   "TAYLOR'S BRIDGE SOW PCAI",
                   'River Front 1 PCAI',
                   'River Front 2 PCAI',
                   'BARWICK SOW PCAI',
                   'ECO SYSTEM 1 PCAI',
                   'TURNER PCAI',
                   'ENVIRO TECH 1 PCAI',
                   'ENVIRO TECH 2 PCAI',
                   'ENVIRO TECH 3 PCAI',
                   'Hairr 1 PCAI',
                   'Heritage PCAI',
                   'TRIAD 1 SOW PCAI',
                   'TRIAD 2 SOW PCAI',
                   'SPGFR',
                   '27041TRAD',
                   '27061TRAD',
                   '2704 TRAD',
                   '2706 TRAD',
                   'GCM TRAD',
                   'Jack Rabbit PCAI',
                   'Pheasant Run PCAI',
                   'USDA'))

nuc9b<-nuc9 %>% 
  filter(!Dest%in%c('103307- WOLF 7 TRAD',
                   '27041',
                   '27041 TRAD',
                   '71921 TRAD',
                   'SPG FARM 60 TRAD',
                   'SPG FARM 61 TRAD',
                   'SPG FARM 62 TRAD',
                   'SPG FARM 63 TRAD',
                   'SPG FARM 64 TRAD',
                   'SPG Farm 62',
                   'TA WOMBLE TRAD',
                   'WOLF 7  (PSFMO) TRAD',
                   'GREEN MEADOWS SOW PCAI',
                   "TAYLOR'S BRIDGE SOW PCAI",
                   'River Front 1 PCAI',
                   'River Front 2 PCAI',
                   'BARWICK SOW PCAI',
                   'ECO SYSTEM 1 PCAI',
                   'TURNER PCAI',
                   'ENVIRO TECH 1 PCAI',
                   'ENVIRO TECH 2 PCAI',
                   'ENVIRO TECH 3 PCAI',
                   'Hairr 1 PCAI',
                   'Heritage PCAI',
                   'TRIAD 1 SOW PCAI',
                   'TRIAD 2 SOW PCAI',
                   'SPGFR',
                   '27041TRAD',
                   '27061TRAD',
                   '2704 TRAD',
                   '2706 TRAD',
                   'GCM TRAD',
                   'Jack Rabbit PCAI',
                   'Pheasant Run PCAI',
                   'USDA'))

nuc9c<-anti_join(x = nuc9b,y = nuc9a,by=c("BoarID"="BoarID"))

nuc9c<-nuc9c[!duplicated(nuc9c[c("Boar Stud.x","Collnum")]), ]

nuc10<-nuc9c %>% 
  mutate('Collected'=ifelse(`Collection Status`%in%c('US','TR'),'YES', 'NO'),
         'Distributed'=ifelse(is.na(Dest),'NO','YES'))

nuc11<-nuc10[c(1:4,10,11,33,17,34,8)]

nuc11<-nuc11[order(nuc11$`Boar Stud`,nuc11$Breed.x,-nuc11$Index),]

write_csv(x = nuc11,path = here::here("data","nucdown.csv"),append = FALSE)

#########################################################################

nuc11<-nuc11[order(nuc11$BoarID,nuc11$`Collection Status`),]

alloc1<-alloc %>% 
  filter(SelDate<=floor_date(x = today(),unit = "week",week_start = 1)-7,
         UnSelDate>=floor_date(x = today(),unit = "week",week_start = 1) | is.na(UnSelDate))

nuc11a<-nuc11 %>%
  filter(`Boar Stud`%in%studs)

nuc11b<-left_join(x = nuc11a,y = alloc1,by=c("Boar Stud"="Boar Stud","BoarID"="Tattoo"))

write_csv(x = nuc11b, path = here::here("data","nucleus.csv"),append = FALSE)

nuc12<-nuc11b %>%
  group_by(BoarID) %>% 
  mutate(dr=row_number(Col_Date),
         count=rowsum(dr,group = BoarID))

nuc13<-nuc12 %>% 
  group_by(BoarID) %>% 
  mutate('Included'=ifelse(`Boar Status`=='NONWORKING', 'NO',
                           ifelse(Target%in%c('CTH-Semen Test','SS-Semen Test'),'NO',
                                  ifelse(BoarID%in%c(),'NO',
                                         ifelse(dr==max(dr) | is.na(dr),'YES','NO')))))

nuc14<-nuc13 %>% 
  filter(Included=='YES',
         Distributed=='YES' | `Collection Status`=='TR')

nuc14<-nuc14[order(nuc14$`Boar Stud`,nuc14$Breed.x,-nuc14$Index),]

nuc15<-nuc14 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  filter(!is.na(Index)) %>% 
  mutate(rank=row_number(-Index),
         maxrank=ifelse(length(Distributed[Distributed=='YES'])==0,0,max(rank[Distributed=='YES'])))

nuc16<-nuc15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  summarise(actual=ifelse(mean(maxrank)==0,0,mean(Index[rank<=maxrank])))

nuc17<-nuc15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  summarize('Needed'=mean(maxrank))

nuc18<-nuc13 %>% 
  filter(Included=='YES')

nuc19<-left_join(x = nuc18,y = nuc17,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))

nuc20<-nuc19 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  filter(!is.na(Index)) %>% 
  mutate(rank=row_number(-Index)) %>% 
  summarise(target=ifelse(mean(Needed)==0,0,mean(Index[rank<=Needed])))

nuc21<-left_join(x = nuc16,y = nuc20,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))

nuc21$`Missed Index`<-nuc21$target-nuc21$actual

nuc22<-nuc19 %>%
  group_by(`Boar Stud`, Breed.x) %>% 
  mutate(wc=floor_date(Col_Date,unit = "week",week_start = 1)) %>%  
  filter(!is.na(wc)) %>% 
  summarise(WeekCommencing=max(wc))

nuc23<-left_join(x = nuc22,y = nuc21,by=c("Boar Stud"="Boar Stud", "Breed.x"="Breed.x"))

nuc24<-nuc23 %>% 
  filter(Breed.x==ifelse(`Boar Stud`=='MBW Illinois','SPG120','SPG240'))

nuc24<-nuc24[c(-2)]

nuc24[is.na(nuc24)]<-0

write_csv(x = nuc24,path = here::here("data","topn_nuc.csv"), append = FALSE)

# write_csv(x = nuc23,path = here::here("data","missedindexall.csv"), append = TRUE, col_names = TRUE)

################
nuc25<-nuc15 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate("Actual"=ifelse(rank<=maxrank,1,0))

nuc26<-nuc19 %>% 
  group_by(`Boar Stud`,Breed.x) %>% 
  mutate(rank=row_number(-Index)) %>% 
  mutate('Target'=ifelse(rank<=Needed,1,0))


nuc27<-left_join(x = nuc13,y = nuc17,by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))
nuc28<-left_join(x = nuc27,y = nuc25,by=c("Boar Stud"="Boar Stud","Collnum"="Collnum"))

nuc29<-nuc28

nuc29<-nuc29[c(1:4,6:11,16,17,34)]

nuc29<-nuc29[order(nuc29$Breed.x.x,-nuc29$Index.x),]

write_csv(x = nuc29,path = here::here("data","nuc.csv"), append = FALSE)

spgn<-nuc29 %>% filter(`Boar Stud`=='SPGNC')
spgv<-nuc29 %>% filter(`Boar Stud`=='SPGVA')
spgt<-nuc29 %>% filter(`Boar Stud`=='SPGTX')
aski<-nuc29 %>% filter(`Boar Stud`=='MB 7092')

write_xlsx(x=list("SPG NC"=spgn,
                  "SPG VA"=spgv,
                  "SPG TX"=spgt,
                  "Askin 7092"=aski),
           here::here("data","MissedIndex_Breakdown_Nucleus.xlsx"))

################# Weighted by line#################

# nuc30<-left_join(x = nuc23,y = nuc17, by=c("Boar Stud"="Boar Stud","Breed.x"="Breed.x"))
# 
# nuc31<-nuc30 %>%
#   group_by(`Boar Stud`) %>%
#   filter(Breed.x%in%c('SPG110','SPG120','SPG240')) %>%
#   mutate(totalneeded=sum(Needed))
# 
# nuc31$missed_partial<-nuc31$`Missed Index`*(nuc31$Needed/nuc31$totalneeded)
# 
# nuc32<-nuc31 %>%
#   group_by(`Boar Stud`) %>%
#   mutate(missed=sum(missed_partial)) %>% 
#   filter(Breed.x==ifelse(`Boar Stud`=='MB 7092','SPG240','SPG110'))
# 
# nuc32<-nuc32[c(1,3,4,10,6)]
# 
# nuc32$`Missed Index`<-nuc32$missed
# 
# nuc33<-nuc32[c(1,2,3,6,5)]
# 
# write_csv(x = nuc33,path = here::here("data","topn_nuc.csv"), append = FALSE)
