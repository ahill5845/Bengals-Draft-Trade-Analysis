library(dplyr)
library(readr)
library(tidyr)
library(leaps)
library(factoextra)
library(ggplot2)
setwd('C:\\Users\\hilla\\OneDrive\\Documents\\Bengals Data\\clusteranalysis')
rank <- read.csv('TeamRankData.csv')
score <- read.csv('TeamScoreData.csv')
#### Team Trade Value Score ----
rownames(score) <- score$Team
score.1 <- score %>% 
  select(.,-Team)

km.score <- kmeans(score.1, centers = 9, nstart = 25)
km.score
fviz_cluster(km.score, data = score.1, labelsize = 8)
km.score$size


