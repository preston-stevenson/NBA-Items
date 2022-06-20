#install.packages("devtools")
library(devtools)

#devtools::install_github("abresler/nbastatR")
library(nbastatR)
library(dplyr)
#1
# box_scores(game_ids = 22100951:22101000, 
#            box_score_types = c("Traditional", "Advanced"), 
#            result_types = c("player", "team"), 
#            join_data = TRUE, 
#            assign_to_environment = TRUE, 
#            return_message = TRUE)
# PlayerGames_951_1000 <- dataBoxScorePlayerNBA
# TeamGames_951_1000 <- dataBoxScoreTeamNBA
# 
# PlayerStats <- rbind(aPlayerGames_001_050, aPlayerGames_051_100, aPlayerGames_101_150, 
#                          PlayerGames_151_200, PlayerGames_201_250, PlayerGames_251_300, 
#                          PlayerGames_301_350, PlayerGames_351_400, PlayerGames_401_450,
#                          PlayerGames_451_500, PlayerGames_501_550, PlayerGames_551_600,
#                          PlayerGames_601_650, PlayerGames_651_700, PlayerGames_701_750,
#                          PlayerGames_751_800, PlayerGames_801_850, PlayerGames_851_900,
#                          PlayerGames_901_950, PlayerGames_951_1000)
# 
# 
# TeamStats <- rbind(aTeamGames_001_050, aTeamGames_051_100, aTeamGames_101_150,
#                        TeamGames_151_200, TeamGames_201_250, TeamGames_251_300,
#                        TeamGames_301_350, TeamGames_351_400, TeamGames_401_450,
#                        TeamGames_451_500, TeamGames_501_550, TeamGames_551_600,
#                        TeamGames_601_650, TeamGames_651_700, TeamGames_701_750,
#                        TeamGames_751_800, TeamGames_801_850, TeamGames_851_900,
#                        TeamGames_901_950, TeamGames_951_1000)
# PlayerStats2021 <- PlayerStats
# TeamStats2021 <- TeamStats
# 
# exec <- TeamStats2021 %>%
#   filter(treb < 1)
# 
# write.csv(PlayerStats, "/Users/prestonstevenson/Documents/PlayerStats.csv", row.names = TRUE)
# write.csv(TeamStats, "/Users/prestonstevenson/Documents/TeamStats.csv", row.names = TRUE)

PlayerStats2021 <- read.csv("/Users/prestonstevenson/Documents/PlayerStats.csv")
TeamStats2021 <- read.csv("/Users/prestonstevenson/Documents/TeamStats.csv")


pos <- c('PG','SG','SF','PF','C','PG-SG','SG-PG','SG-SF','PF-SF','SF-SG','C-PF', NA)
pos_val <- c(1, 2, 3, 4, 5, 1.5, 1.5, 2.5, 3.5, 2.5, 4.5, 3)
pos_value <- as.data.frame(cbind(pos,pos_val))

Player_PositionsMaster2021 <- read.csv("/Users/prestonstevenson/Documents/2021_Player_Pos_Age.csv")
Player_Positions2021 <- Player_PositionsMaster2021
colnames(Player_Positions2021) <- c("namePlayer", "Position", "Age")
Player_Positions2021 <- distinct(Player_Positions2021, .keep_all = FALSE)
Player_Positions2021 <- merge(x=Player_Positions2021, y=pos_value, by.x = "Position", by.y = "pos", all.x = TRUE) 
Player_Positions2021 <- Player_Positions2021 %>%
  select(namePlayer, Age, pos_val) %>%
  group_by(namePlayer) #%>%
 # summarize(pos_val = sum(pos_val))



PlayerStats2021 <- merge(x=PlayerStats2021, y=Player_Positions2021, by = "namePlayer", all.x = TRUE)
PlayerStats2021$pos_val[is.na(PlayerStats2021$pos_val)] <- 3
PlayerStats2021$pos_val <- as.numeric(PlayerStats2021$pos_val)
PlayerStats2021[PlayerStats2021 == "DeAndre' Bembry"] <- 'DeAndre Bembry'
  
  