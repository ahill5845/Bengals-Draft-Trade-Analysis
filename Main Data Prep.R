library(dplyr)
library(tidyverse)
library(ggplot2)

setwd('C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtimetrades')
megaTable <- read.csv('BengalsMegaTable.csv')

#### Subsetting Columns ----
filteredTable <- megaTable %>%
  select(.,-SLOTCLUB, -TRADESTATUS, -NFL_PLAYERID, -HASCONDITIONS, -CONDITIONSEVALUATED, 
         -TRADEVOID, -TRADEDATE, -SEASON, -FOOTBALLNAME, -COLLEGE, -HEIGHT, -WEIGHT,
         -BIRTHDATE, -DRAFTROUND, -DRAFTNUMBER, -DRAFTPICKINROUND, -DRAFTCLUB, 
         -ENTERUNDERCLASSMAN, -SUPPLEMENTALDRAFT, -NFL_RECORDTIMESTAMP, -SEASON.1,
         -CLUB, -ENTRYYEAR, -SIDEOFBALL_PCT) %>% 
  group_by(TRADEID)

tradeTable <- filteredTable %>% 
  distinct(TRADEID, .keep_all = TRUE)

tradeTable$NFLEXPERIENCE <- as.integer(tradeTable$NFLEXPERIENCE)
tradeTable$NUM <- 1

#### Which rounds are traded down into the most ----
numDown <- tradeTable %>% 
  select(SELECTION_ROUND, DOWNCLUB) %>% 
  group_by(SELECTION_ROUND) %>% 
  count(DOWNCLUB, name = 'numTimes', sort = FALSE) %>% 
  na.omit()
#### Which rounds are traded up the most ----
#### Total number of players selected per round ----
numSelected <- tradeTable %>% 
  select(SELECTION_ROUND, NUM) %>% 
  group_by(SELECTION_ROUND) %>% 
  count(NUM, name = 'PlayersSelected', sort = FALSE) %>% 
  na.omit()

numSelected <- numSelected %>% select(.,-NUM)

#### Total playing time in the nfl per round ----
ptround <- tradeTable %>% 
  select(SELECTION_ROUND, NFLEXPERIENCE, GS, GA, SOB_NP, ST_NP) %>%
  group_by(SELECTION_ROUND) %>% 
  summarise_at(c("NFLEXPERIENCE", "GS", "GA", "SOB_NP", "ST_NP"), mean, na.rm = TRUE) %>%
  na.omit()

#### Total number of players selected by UPCLUB ----
upclubSelected <- tradeTable %>% 
  select(UPCLUB, SELECTCLUB, NUM) %>% 
  group_by(UPCLUB) %>%
  filter(UPCLUB == SELECTCLUB, UPCLUB != '') %>% 
  count(NUM, name = 'PlayersSelected', sort = FALSE) %>% 
  na.omit()
  
upclubSelected <- upclubSelected %>% select(.,-NUM) 
#two teams never selected a pick when trading up

#### Total playing time in the NFL by UPCLUB ----
ptUpclub <- tradeTable %>% 
  select(UPCLUB, SELECTCLUB, NFLEXPERIENCE, GS, GA, SOB_NP, ST_NP) %>%
  group_by(UPCLUB) %>% 
  filter(UPCLUB == SELECTCLUB, UPCLUB != '') %>% 
  summarise_at(c("NFLEXPERIENCE", "GS", "GA", "SOB_NP", "ST_NP"), mean, na.rm = TRUE) %>%
  na.omit()

#### Total number of players selected by DOWNCLUB ----
downclubSelected <- tradeTable %>% 
  select(DOWNCLUB, SELECTCLUB, NUM) %>% 
  group_by(DOWNCLUB) %>%
  filter(DOWNCLUB == SELECTCLUB, DOWNCLUB != '') %>% 
  count(NUM, name = 'PlayersSelected', sort = FALSE) %>% 
  na.omit()

downclubSelected <- downclubSelected %>% select(.,-NUM) 

#### Total playing time in the NFL by DOWNCLUB ----
ptDownclub <- tradeTable %>% 
  select(DOWNCLUB, SELECTCLUB, NFLEXPERIENCE, GS, GA, SOB_NP, ST_NP) %>%
  group_by(DOWNCLUB) %>% 
  filter(DOWNCLUB == SELECTCLUB, DOWNCLUB != '') %>% 
  summarise_at(c("NFLEXPERIENCE", "GS", "GA", "SOB_NP", "ST_NP"), mean, na.rm = TRUE) %>%
  na.omit()

#### Write all dataframes to csv ----
roundStats <- merge(numSelected, ptround, by = 'SELECTION_ROUND')
write.csv(roundStats, 'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtimetrades\\roundStats.csv')

upStats <- merge(upclubSelected, ptUpclub, by = 'UPCLUB')
write.csv(upStats, 'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtimetrades\\upStats.csv')

downStats <- merge(downclubSelected, ptDownclub, by = 'DOWNCLUB')
write.csv(downStats, 'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtimetrades\\downStats.csv')












