setwd('C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data')
library(tidyverse)
library(dplyr)
library(ggplot2)

trades = read.csv('drafttrades.csv')
player = read.csv('playermaster.csv')
playingTime = read.csv('playingtime.csv')

#1. FREQUENCY OF TRADES PER ROUND
roundFreq = read.csv('roundFrequency.csv')
write.csv(roundFreq,'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\roundFreqStr.csv', row.names = FALSE)

#2. distribution of trades that include a current player vs. trades that don't


#3. number of trades involving players per round 
#   - dummy variable where involving a player = 1 and not involving a player = 0
#   - export the table with the dummy variable as a csv to then put it into SQL
includesPlayers <- trades %>% 
  select()


#4. which position group is traded the most
trades <- trades %>% 
  rename(PLAYERID = NFL_PLAYERID)

player$PLAYERID = as.integer(player$PLAYERID)

playerTrade <-  inner_join(player, trades, by = 'PLAYERID', na.action=na.omit)
playerTrade <- playerTrade %>% 
  select(.,-FIRSTNAME,-COLLEGE,-HEIGHT,-WEIGHT,-BIRTHDATE)

write.csv(playerTrade,'C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\playerTrade.csv', row.names = FALSE)

#5. trade that involve future picks
futurePicksTrades <- trades %>% 
  filter(INCLUDESFUTUREPICKS == 1) #subset of trades that involve future picks








