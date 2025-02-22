library(dplyr)
library(tidyverse)
library(ggplot2)

setwd('C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtime')
PlayingTable <- read.csv('PlayerPlayingTime.csv')

PTTable <- PlayingTable %>% 
  distinct(PLAYERID, .keep_all = TRUE)
PTTable$NUM <- 1
#### Number of players per position ----
CountPos <- PTTable %>% 
  select(POSITION) %>% 
  group_by(POSITION) %>% 
  summarise(n())
#### Playing Time Per Position ----
##Offense and Defense## 
PTPos <- PTTable %>% 
  select(POSITION, SOB_NP) %>% 
  group_by(POSITION) %>% 
  summarise(totalSnaps = sum(SOB_NP, na.rm = TRUE)) 
PTGS <- PTTable %>% 
  select(POSITION, GS) %>% 
  group_by(POSITION) %>% 
  summarise(totalGS = sum(GS, na.rm = TRUE)) 
PosPT <- inner_join(CountPos, PTGS, by = 'POSITION')
NewPosPT <- inner_join(PosPT, PTPos, by = 'POSITION')
##Special Teams##
STPos <- PTTable %>% 
  select(POSITION, ST_NP) %>% 
  group_by(POSITION) %>%
  filter(POSITION == 'LS' | POSITION == 'PK' | POSITION == 'PT') %>% 
  summarise(totalSnaps = sum(ST_NP, na.rm = TRUE))
STGA <- PTTable %>% 
  select(POSITION, GA) %>% 
  group_by(POSITION) %>% 
  filter(POSITION == 'LS' | POSITION == 'PK' | POSITION == 'PT') %>% 
  summarise(totalGA = sum(GA, na.rm = TRUE)) 
NewPosST <- inner_join(STPos, STGA, by = 'POSITION')
write.csv(NewPosPT,'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtime\\PositionSnapsGames.csv')
write.csv(NewPosST,'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtime\\STSnapsGames.csv')
#### Number of players per round ----
CountRound <- PTTable %>% 
  select(DRAFTROUND) %>% 
  group_by(DRAFTROUND) %>% 
  summarise(n())
#### Playing Time Per Round ----
#Snaps 
PTround <- PTTable %>% 
  select(DRAFTROUND, SOB_NP) %>% 
  group_by(DRAFTROUND) %>% 
  summarise(totalSnaps = sum(SOB_NP, na.rm = TRUE)) 
#Games started
GSround <- PTTable %>% 
  select(DRAFTROUND, GS) %>% 
  group_by(DRAFTROUND) %>% 
  summarise(GamesStarted = sum(GS, na.rm = TRUE)) 
RoundCount <- inner_join(CountRound, PTround, by = 'DRAFTROUND')
NewRoundCount <- inner_join(RoundCount, GSround, by = 'DRAFTROUND')
write.csv(NewRoundCount,'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtime\\RoundCount.csv')

#### OVERALL TRENDS ----
CountPlayers <- PTTable %>% 
  select(DRAFTYEAR, DRAFTROUND, POSITION, GS, SOB_NP) %>%
  group_by(DRAFTYEAR, DRAFTROUND, POSITION) %>% 
  summarise(
    Games_started = sum(GS, na.rm = TRUE),
    Snaps_played = sum(SOB_NP, na.rm = TRUE)
  )
CountDrafted <- PTTable %>% 
  select(DRAFTYEAR, DRAFTROUND, POSITION) %>% 
  group_by(DRAFTYEAR, DRAFTROUND, POSITION) %>% 
  summarise(n())

write.csv(CountPlayers,'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtime\\CountPlayers.csv')
write.csv(CountDrafted,'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playingtime\\CountDrafted.csv')
















