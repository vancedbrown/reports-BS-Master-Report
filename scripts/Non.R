library(tidyverse)
library(readr)
library(here)


studs<-c('High Desert',
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
         'SPGTX',
         'Norson',
         'GCM7-4',
         'Clinton',
         'Ingold',
         'Prestage MS',
         'MB 7083',
         'MB 7084')

source('C:/Users/vance/Documents/myR/functions/getSQL.r')


pig <- "SELECT [StudID] AS 'Boar Stud'
,[BoarID]
,[Breed]
,[Status] AS 'Boar Status'
,[Date_Arrival]
,[Date_Studout]
,[Dispose_Code]
,[Reason_Studout]
FROM [Intranet].[dbo].[Boar_Pig]"
non1 <- getSQL('Intranet',query=pig)

collraw<-read_csv("C:/Users/vance/Documents/Working Project Directory/data/data-SQL-BS-Data-Pull/coll.csv")
collraw$Col_Date<-as.Date(collraw$Col_Date)

non2<-non1 %>% 
  filter(`Boar Status`=='NONWORKING')

non3<-left_join(x = non2,y = collraw,by=c("BoarID"="BoarID"))

non3$date<-as.Date(non3$Col_Date)

non4<-non3 %>% 
  group_by(BoarID) %>% 
  mutate(date.rank = row_number(date))

non5<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-9)
non5$`1`<-non5$`Collection Status`
non5<-non5[c(2,27)]

non6<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-8)
non6$`2`<-non6$`Collection Status`
non6<-non6[c(2,27)]

non7<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-7)
non7$`3`<-non7$`Collection Status`
non7<-non7[c(2,27)]

non8<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-6)
non8$`4`<-non8$`Collection Status`
non8<-non8[c(2,27)]

non9<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-5)
non9$`5`<-non9$`Collection Status`
non9<-non9[c(2,27)]

non10<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-4)
non10$`6`<-non10$`Collection Status`
non10<-non10[c(2,27)]

non11<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-3)
non11$`7`<-non11$`Collection Status`
non11<-non11[c(2,27)]

non12<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-2)
non12$`8`<-non12$`Collection Status`
non12<-non12[c(2,27)]

non13<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank)-1)
non13$`9`<-non13$`Collection Status`
non13<-non13[c(2,27)]

non14<-non4 %>% 
  group_by(BoarID) %>% 
  filter(date.rank==max(date.rank))
non14$`10`<-non14$`Collection Status`
non14<-non14[c(2,27)]

non15<-non3 %>% 
  group_by(BoarID) %>% 
  summarize('Last Collection Date'=max(date))

non16<-left_join(x = non2,y = non5,by=c("BoarID"="BoarID"))
non17<-left_join(x = non16,y = non6,by=c("BoarID"="BoarID"))
non18<-left_join(x = non17,y = non7,by=c("BoarID"="BoarID"))
non19<-left_join(x = non18,y = non8,by=c("BoarID"="BoarID"))
non20<-left_join(x = non19,y = non9,by=c("BoarID"="BoarID"))
non21<-left_join(x = non20,y = non10,by=c("BoarID"="BoarID"))
non22<-left_join(x = non21,y = non11,by=c("BoarID"="BoarID"))
non23<-left_join(x = non22,y = non12,by=c("BoarID"="BoarID"))
non24<-left_join(x = non23,y = non13,by=c("BoarID"="BoarID"))
non25<-left_join(x = non24,y = non14,by=c("BoarID"="BoarID"))
non26<-left_join(x = non25,y = non15,by=c("BoarID"="BoarID"))

non26<-non26[c(1:3,9:19,8)]

write_csv(x = non26,path = here::here("data","non.csv"),append = FALSE)
