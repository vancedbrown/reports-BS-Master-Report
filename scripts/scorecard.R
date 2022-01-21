

## ---- test ----


library(shiny) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(reshape)
library(lubridate)
library(grid)
library(scales)
library(sqldf)
library(taRifx)
library(readr)
library(knitr)


## ---- test1 ----
raw=read_csv("//ncrh1/users/Vance/Semen QC IVOS Data/Semen QC Master-Updated (Fix01122016).csv",  na = c(".", "", "NA", "NaN"))

nrow(raw) 
names(raw)

names(raw)[names(raw) == "Conc_Bill/mL"] <- "Conc_Bill_mL"
names(raw)[names(raw) == "%Normal_Morph"] <- "pct_Normal_Morph"
names(raw)[names(raw) == "%Motility"]     <- "pct_Motility"

df2= raw %>%  select(
  Year,             
  Week,             
  StudNum,          
  StudName,         
  Bacteria_CFUs, 
  Conc_Bill_mL, 
  pct_Normal_Morph,
  pct_Motility)


df_in=df2 %>% filter(StudName %in% c(
  'ASKI',
  'BLAD',
  'CIMA',
  'CLIN',
  'CYCL',
  'CIAN',
  'DEER',
  'DOVE',
  'ELIZ',
  'GOLD',
  'ILLI',
  'INGO',
  'HIGH', 
  'LAUR',
  'MACO',
  'PRIN',
  'ROBE',
  'SKYL',
  'SPGN',
  'SPGT',
  'SPGV',
  'WILL',
  'YUMA',
  'MISS',
  'SIER',
  '7362'),
  Year>='2016'
)

df_in$YW=df_in$Year*100 + df_in$Week

min_YW=min(df_in$YW)
max_YW=max(df_in$YW)


nbatch=df_in %>% 
  select(YW) %>% filter(!duplicated(YW)) %>% arrange(YW) %>%
  mutate(n_wk = (1:length(YW)),
         Year_week = YW) %>% 
  arrange(Year_week)

max=max(nbatch$n_wk)

df_all=df_in %>% 
  mutate(Year_week = YW) %>% 
  arrange(Year_week)     %>%
  left_join(nbatch, by = c("Year_week", "YW")) %>%
  
  mutate(Bacteria=replace(Bacteria_CFUs, Bacteria_CFUs=='NC', NA),
         Conc_Bill = Conc_Bill_mL
        ) 


df_all_data= df_all %>%  
  
  mutate(Bacteria_n=round(as.numeric(Bacteria)),
         Conc_Bill_n=round(Conc_Bill),
         pct_Normal_Morph_n=round(pct_Normal_Morph)                   
         ) %>%
  
  mutate(Bacteria_NC = replace(Bacteria_n, Bacteria_n !='NA', 1)) %>%                        
  
  mutate(Bacteria_150=ifelse(Bacteria_n >=3, 1,0))


####sum
df_sum1=df_all_data %>%
  group_by(StudNum, StudName, Year_week, n_wk) %>%
  dplyr::summarise(total1=n(),
            AVG_Conc_Bill=round(mean(Conc_Bill_n,na.rm=TRUE),0),
            SD_Conc_Bill=round(sd(Conc_Bill_n,na.rm=TRUE),1),
            AVG_Normal_Morph=round(mean(pct_Normal_Morph_n,na.rm=TRUE),0),
            min_Normal_Morph=round(min(pct_Normal_Morph_n),0),
            B_150=sum(Bacteria_150),
            B_NA=sum(Bacteria_NC),
            AVG_Motility=round(mean(pct_Motility,na.rm=TRUE),0)
  ) %>%
  
  mutate(Counts=as.double(total1)) %>%  
  select(StudNum, StudName, Year_week, n_wk, Counts, AVG_Conc_Bill, SD_Conc_Bill, AVG_Normal_Morph, min_Normal_Morph, B_150, B_NA, AVG_Motility) %>% 
  arrange(StudNum, StudName, Year_week, n_wk)  

df_sum2= df_sum1 %>% 
  mutate(pct_Bacteria= round((B_150/B_NA *100),2))


df_sum2$StudNum

####### targets
df_out= df_sum2 %>% 
  mutate( AVG_CB_m = ifelse(StudNum %in% c(1:16,21,22,23,24), 44.0,
                            ifelse(StudNum %in% c(17,18), 34.7, 
                                   ifelse(StudNum %in% c(19), 44.0,     
                                          ifelse(StudNum %in% c(20), 31.25, 0)))),
          AVG_CB_df=abs(round((AVG_Conc_Bill - AVG_CB_m),2))
  )


df_out[is.na(df_out)] <- 0



df_score=df_out %>%
  mutate(AVG_CB_score = ifelse(AVG_CB_df <= 2, 3, 
                               ifelse(AVG_CB_df > 2 & AVG_CB_df <=4, 2,
                                      ifelse(AVG_CB_df > 4 & AVG_CB_df <=6, 1, 0)))) %>%
  
  mutate(SD_CBill_score = ifelse(SD_Conc_Bill <= 3, 3, 
                                 ifelse(SD_Conc_Bill > 3 & SD_Conc_Bill <=6, 2,
                                        ifelse(SD_Conc_Bill > 6 & SD_Conc_Bill <=9, 1, 0)))) %>% 
  
  mutate(AVG_NM_score = ifelse(AVG_Normal_Morph >= 75.6, 3, 
                               ifelse(AVG_Normal_Morph >=74.6 & AVG_Normal_Morph <=75.5, 2,
                                      ifelse(AVG_Normal_Morph < 74.6, 1, 0))))  %>%  
  
  mutate(min_NM_score = ifelse(min_Normal_Morph >= 70, 3, 
                               ifelse(min_Normal_Morph <= 69 & min_Normal_Morph >= 65, 2,
                                      ifelse(min_Normal_Morph <= 64 & min_Normal_Morph >= 60, 1, 0)))) %>% 
  
  mutate(pct_B_score = ifelse(pct_Bacteria <= 10, 3, 
                              ifelse(pct_Bacteria > 10 & pct_Bacteria <= 20, 2,
                                     ifelse(pct_Bacteria > 20 & pct_Bacteria <= 30, 1, 0)))) %>%
  
  mutate(Comb_score = round((AVG_CB_score + SD_CBill_score + AVG_NM_score + min_NM_score + pct_B_score)*1.0,2))


card0<-read_csv('C:/Users/vance/Documents/projects/Working Project Directory/reports/reports-BS-Master-Report/inputs/months.csv')
card1<-left_join(x = df_score,y = card0,by=c("Year_week"="Week"))
write_csv(x = card1, path = 'C:/Users/vance/Documents/projects/Working Project Directory/reports/reports-BS-Master-Report/data/scarddata.csv',append = FALSE)

#######################################

card<-df_score %>% 
  filter(Year_week==max_YW)

card2<-df_score %>% 
  group_by(StudNum) %>% 
  top_n(5,Year_week) %>% 
  filter(Year_week!=max_YW) %>%
  dplyr::summarize(wk=mean(Comb_score))

card3<-df_score %>% 
  group_by(StudNum) %>% 
  top_n(13,Year_week) %>% 
  dplyr::summarize(q=mean(Comb_score))

card<-card[c(1,2,5,6,15,16,7,17,8,18,9,19,13,20,21)]

card4<-left_join(x = card,y = card2,by=c("StudNum"="StudNum"))
card5<-left_join(x = card4,y = card3,by=c("StudNum"="StudNum"))

order.score<-order(card5$wk,card5$Comb_score,decreasing = TRUE)
card6<-card5[order.score,] 
card7<-card6 %>% 
  filter(StudNum%in%c(1:16,21:24))


write_csv(x = card7,path = 'C:/Users/vance/Documents/projects/Working Project Directory/reports/reports-BS-Master-Report/data/scard.csv',append = FALSE)


