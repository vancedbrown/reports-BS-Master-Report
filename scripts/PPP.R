library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(here)


library(readr)
batchraw <- read_csv("C:/Users/vance/Documents//projects/Working Project Directory/data/data-SQL-BS-Data-Pull/batch.csv", 
                  col_types = cols(POST_MOT_VAL_1 = col_number(), 
                                   POST_MOT_VAL_2 = col_number(), 
                                   POST_MOT_VAL_3 = col_number(), 
                                   POST_MOT_VAL_4 = col_number(), 
                                   POST_MOT_VAL_5 = col_number(), 
                                   POST_MOT_VAL_6 = col_number()))

distraw<-read_csv("C:/Users/vance/Documents/projects/Working Project Directory/data/data-SQL-BS-Data-Pull/dist.csv")

distraw$Date_Shipped<-as.Date(distraw$Date_Shipped)

batch1<-batchraw %>% 
  filter(BATCHNUM>20000000000) %>% 
  mutate(date=as.Date(COL_DATE))

ppp1<-distraw %>% 
  filter(!Dest%in%c('* TRASH *','SPGFR', 'USDA_FROZEN', 'USDA_EXT','USDA_FR')) %>% 
  filter(!is.na(Dest))

ppp2<-left_join(x = ppp1,y = batch1,by=c("Boar Stud"="Boar Stud","BatchNum"="BATCHNUM"))


ppp3<-ppp2 %>% 
  filter(date<floor_date(x = today(),unit = "week",week_start = 1)) %>% 
  group_by(`Boar Stud`) %>% 
  filter(!is.na(date)) %>% 
  filter(date<max(date)-6,
         date>max(date)-13)

ppp3<-ppp3[-2]

ppp4<-ppp3 %>% 
  group_by(`Boar Stud`) %>% 
  filter(!duplicated(BatchID))

ppp27<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Batches'=n_distinct(BatchNum))

#ppp4$POST_MOT_VAL_1<-as.numeric(levels(ppp4$POST_MOT_VAL_1))[ppp4$POST_MOT_VAL_1]

#ppp4$POST_MOT_VAL_2<-as.numeric(levels(ppp4$POST_MOT_VAL_2))[ppp4$POST_MOT_VAL_2]


ppp4$d1<-ppp4$POST_MOT_VAL_2-ppp4$POST_MOT_VAL_1
ppp4$d2<-ppp4$POST_MOT_VAL_3-ppp4$POST_MOT_VAL_2
ppp4$d3<-ppp4$POST_MOT_VAL_4-ppp4$POST_MOT_VAL_3
ppp4$d4<-ppp4$POST_MOT_VAL_5-ppp4$POST_MOT_VAL_4
ppp4$d5<-ppp4$POST_MOT_VAL_6-ppp4$POST_MOT_VAL_5

# ppp5<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('-25%'=20*sum(length(which(between(d1,-90,-25.0))),
#                           length(which(between(d2,-90,-25.0))),
#                           length(which(between(d3,-90,-25.0))),
#                           length(which(between(d4,-90,-25.0))),
#                           length(which(between(d5,-90,-25.0))))/length(BatchNum))
# 
# ppp6<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('-20%'=20*sum(length(which(between(d1,-24.9,-20.0))),
#                           length(which(between(d2,-24.9,-20.0))),
#                           length(which(between(d3,-24.9,-20.0))),
#                           length(which(between(d4,-24.9,-20.0))),
#                           length(which(between(d5,-24.9,-20.0))))/length(BatchNum))
# 
# ppp7<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('-15%'=20*sum(length(which(between(d1,-19.9,-15.0))),
#                           length(which(between(d2,-19.9,-15.0))),
#                           length(which(between(d3,-19.9,-15.0))),
#                           length(which(between(d4,-19.9,-15.0))),
#                           length(which(between(d5,-19.9,-15.0))))/length(BatchNum))
# 
# ppp8<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('-10%'=20*sum(length(which(between(d1,-14.9,-10.0))),
#                           length(which(between(d2,-14.9,-10.0))),
#                           length(which(between(d3,-14.9,-10.0))),
#                           length(which(between(d4,-14.9,-10.0))),
#                           length(which(between(d5,-14.9,-10.0))))/length(BatchNum))
# 
# ppp9<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('-5%'=20*sum(length(which(between(d1,-9.9,-5.0))),
#                          length(which(between(d2,-9.9,-5.0))),
#                          length(which(between(d3,-9.9,-5.0))),
#                          length(which(between(d4,-9.9,-5.0))),
#                          length(which(between(d5,-9.9,-5.0))))/length(BatchNum))
# 
# ppp10<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('0%'=20*sum(length(which(between(d1,-4.9,0.0))),
#                         length(which(between(d2,-4.9,0.0))),
#                         length(which(between(d3,-4.9,0.0))),
#                         length(which(between(d4,-4.9,0.0))),
#                         length(which(between(d5,-4.9,0.0))))/length(BatchNum))
# 
# ppp11<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('+5%'=20*sum(length(which(between(d1,0.1,5.0))),
#                          length(which(between(d2,0.1,5.0))),
#                          length(which(between(d3,0.1,5.0))),
#                          length(which(between(d4,0.1,5.0))),
#                          length(which(between(d5,0.1,5.0))))/length(BatchNum))
# 
# ppp12<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('+10%'=20*sum(length(which(between(d1,5.1,10.0))),
#                           length(which(between(d2,5.1,10.0))),
#                           length(which(between(d3,5.1,10.0))),
#                           length(which(between(d4,5.1,10.0))),
#                           length(which(between(d5,5.1,10.0))))/length(BatchNum))
# 
# ppp13<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('+15%'=20*sum(length(which(between(d1,10.1,15.0))),
#                           length(which(between(d2,10.1,15.0))),
#                           length(which(between(d3,10.1,15.0))),
#                           length(which(between(d4,10.1,15.0))),
#                           length(which(between(d5,10.1,15.0))))/length(BatchNum))
# 
# ppp14<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('20%'=20*sum(length(which(between(d1,15.1,20.0))),
#                          length(which(between(d2,15.1,20.0))),
#                          length(which(between(d3,15.1,20.0))),
#                          length(which(between(d4,15.1,20.0))),
#                          length(which(between(d5,15.1,20.0))))/length(BatchNum))
# 
# ppp15<-ppp4 %>% 
#   group_by(`Boar Stud`) %>% 
#   summarize('+25%'=20*sum(length(which(between(d1,20.1,25.0))),
#                           length(which(between(d2,20.1,25.0))),
#                           length(which(between(d3,20.1,25.0))),
#                           length(which(between(d4,20.1,25.0))),
#                           length(which(between(d5,20.1,25.0))))/length(BatchNum))
# 
# ppp16<-left_join(x = ppp27,y = ppp5,by=c("Boar Stud"="Boar Stud"))
# ppp17<-left_join(x = ppp16,y = ppp6,by=c("Boar Stud"="Boar Stud"))
# ppp18<-left_join(x = ppp17,y = ppp7,by=c("Boar Stud"="Boar Stud"))
# ppp19<-left_join(x = ppp18,y = ppp8,by=c("Boar Stud"="Boar Stud"))
# ppp20<-left_join(x = ppp19,y = ppp9,by=c("Boar Stud"="Boar Stud"))
# ppp21<-left_join(x = ppp20,y = ppp10,by=c("Boar Stud"="Boar Stud"))
# ppp22<-left_join(x = ppp21,y = ppp11,by=c("Boar Stud"="Boar Stud"))
# ppp23<-left_join(x = ppp22,y = ppp12,by=c("Boar Stud"="Boar Stud"))
# ppp24<-left_join(x = ppp23,y = ppp13,by=c("Boar Stud"="Boar Stud"))
# ppp25<-left_join(x = ppp24,y = ppp14,by=c("Boar Stud"="Boar Stud"))
# ppp26<-left_join(x = ppp25,y = ppp15,by=c("Boar Stud"="Boar Stud"))
# ppp26[is.na(ppp26)]<-0
# 
# write_csv(x = ppp26,path = here::here("data","ppp.csv"))

####################################################

ppm1<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  filter(TYPE!='3D') %>% 
  summarize(ms1=sum(is.na(POST_MOT_VAL_1),
                    is.na(POST_MOT_VAL_2),
                    is.na(POST_MOT_VAL_3),
                    is.na(POST_MOT_VAL_4),
                    is.na(POST_MOT_VAL_5),
                    is.na(POST_MOT_VAL_6)))
ppm2<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  filter(TYPE!='3D') %>% 
  filter(is.na(POST_MOT_VAL_1)|
           is.na(POST_MOT_VAL_2)|
           is.na(POST_MOT_VAL_3)|
           is.na(POST_MOT_VAL_4)|
           is.na(POST_MOT_VAL_5)|
           is.na(POST_MOT_VAL_6)) %>% 
  summarize(ib1=n_distinct(BatchNum))

ppm3<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  filter(TYPE=='3D') %>% 
  summarize(ms2=sum(is.na(POST_MOT_VAL_1),
                    is.na(POST_MOT_VAL_2),
                    is.na(POST_MOT_VAL_3),
                    is.na(POST_MOT_VAL_4)))
ppm4<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  filter(TYPE=='3D') %>% 
  filter(is.na(POST_MOT_VAL_1)|
           is.na(POST_MOT_VAL_2)|
           is.na(POST_MOT_VAL_3)|
           is.na(POST_MOT_VAL_4)) %>% 
  summarize(ib2=n_distinct(BatchNum))

ppm5<-left_join(x = ppm1,y = ppm3,by=c("Boar Stud"="Boar Stud"))
ppm6<-left_join(x = ppm2,y = ppm4,by=c("Boar Stud"="Boar Stud"))
ppm5[is.na(ppm5)]<-0
ppm6[is.na(ppm6)]<-0
ppm5$`Missing Scores`<-ppm5$ms1+ppm5$ms2
ppm6$`Incomplete Batches`<-ppm6$ib1+ppm6$ib2

ppm7<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Day 0'=sum(is.na(POST_MOT_VAL_1)))
ppm8<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Day 1'=sum(is.na(POST_MOT_VAL_2)))
ppm9<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Day 2'=sum(is.na(POST_MOT_VAL_3)))
ppm10<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  summarize('Day 3'=sum(is.na(POST_MOT_VAL_4)))
ppm11<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  filter(TYPE!='3D') %>% 
  summarize('Day 4'=sum(is.na(POST_MOT_VAL_5)))
ppm12<-ppp4 %>% 
  group_by(`Boar Stud`) %>% 
  filter(TYPE!='3D') %>% 
  summarize('Day 5'=sum(is.na(POST_MOT_VAL_6)))

ppm13<-left_join(x = ppp27,y = ppm6,by=c("Boar Stud"="Boar Stud"))
ppm14<-left_join(x = ppm13,y = ppm5,by=c("Boar Stud"="Boar Stud"))
ppm15<-left_join(x = ppm14,y = ppm7,by=c("Boar Stud"="Boar Stud"))
ppm16<-left_join(x = ppm15,y = ppm8,by=c("Boar Stud"="Boar Stud"))
ppm17<-left_join(x = ppm16,y = ppm9,by=c("Boar Stud"="Boar Stud"))
ppm18<-left_join(x = ppm17,y = ppm10,by=c("Boar Stud"="Boar Stud"))
ppm19<-left_join(x = ppm18,y = ppm11,by=c("Boar Stud"="Boar Stud"))
ppm20<-left_join(x = ppm19,y = ppm12,by=c("Boar Stud"="Boar Stud"))

ppm20[is.na(ppm20)]<-0
ppm20<-ppm20[c(1,2,5,8:14)]

write_csv(x = ppm20,path = here::here("data","ppm.csv"))
