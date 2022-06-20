
library(dplyr)
library(data.table)
library(nbastatR)


#Create table for BPM Coefficients
pos1 <- c(0.86,	-0.56,	-0.2464,	0.389,	0.58,	-0.964,	0.613,	0.116,	0.0,	1.369,	1.327,	-0.367)
pos5 <- c(0.86,	-0.78,	-0.3432,	0.389,	1.034,	-0.964,	0.181,	0.181,	0.0,	1.008,	0.703,	-0.367)
co_bpm <- as.data.frame(rbind(pos1, pos5))
colnames(co_bpm) <- c("Adj. Pt",	"FGA",	"FTA",	"FG3Pt(bonus)",	"AST",	"TO",
                      "ORB", "DRB", "TRB", "STL", "BLK", "PF")

#create table for OBPM Coefficients
opos1 <- c(0.605,	-0.33, -0.1452, 0.477, 0.476, -0.579,	0.606, -0.112, 0.0, 0.177, 0.725, -0.439)
opos5 <- c(0.605, -0.472, -0.20768, 0.477, 0.476, -0.882, 0.422, 0.103, 0.0, 0.294, 0.097, -0.439)
co_obpm <- as.data.frame(rbind(opos1, opos5))
colnames(co_obpm) <- c("Adj. Pt",	"FGA",	"FTA",	"FG3Pt(bonus)",	"AST",	"TO",
                       "ORB", "DRB", "TRB", "STL", "BLK", "PF")

#create table for positions percent for overall and offensive roles
positions <- c(2.130, 8.668, -2.486, 0.992, -3.536, 1.667, 50)
num <- c(1:7)
pos_per <- as.data.frame(rbind(positions,num))
pos_per <- pos_per[1,]
colnames(pos_per) <- c("intercept", "pct_TRB", "pct_STL", "pct_PF", "pct_AST", "pct_BLK", "Min_Wt")

offrole <- c(6.000, -6.642, -8.544, -0.330, 4.000, 50)
num2 <- c(1:6)
off_role_pos <- as.data.frame(rbind(offrole,num2))
off_role_pos <- off_role_pos[1,]
colnames(off_role_pos) <- c("intercept", "pct_ast", "pct_thresh_points", "pt_thresh", "Default_pos", "Min_Wt")

teamlist <- c('MIA', 'ORL', 'ATL', 'CHA', 'WAS',
              'BKN', 'NYK', 'PHI', 'TOR', 'BOS',
              'MIL', 'CHI', 'CLE', 'IND', 'DET',
              'LAL', 'LAC', 'SAC', 'GSW', 'PHX',
              'DAL', 'SAS', 'NOP', 'HOU', 'MEM',
              'UTA', 'DEN', 'POR', 'OKC', 'MIN')

# #Create table for other team for possession data for season
# otherteam <- function(team) {
#   #filter team data to team only
#   a1 <- TeamStats2021 %>%
#     filter(team == slugTeam)
#   #only use the gameID column to get all the games team played in
#   a1 <- a1[,1]
#   #use that vector to get all team data for given gameIDs
#   # a2 <- subset(TeamStats2021, idGame %in% a1)
#   #filter and aggregate those into team stats and opposing teams stats
#   # a3 <- a2 %>%
#   #   filter(team != slugTeam) %>%
#   #   summarize(dreb = mean(dreb), oreb = mean(oreb), fga = mean(fga),
#   #             fta = mean(fta), fgm = mean(fgm), tov = mean(tov), minExact = mean(minExact))
#   # a3 <- a3 %>%
#   #   mutate(TeamName = 'OtherTeam')
#   # a4 <- a2 %>%
#   #   filter(team == slugTeam) %>%
#   #   summarize(dreb = mean(dreb), oreb = mean(oreb), fga = mean(fga),
#   #             fta = mean(fta), fgm = mean(fgm), tov = mean(tov), minExact = mean(minExact))
#   # a4 <- a4 %>%
#   #   mutate(TeamName = 'Team')
#   # a5 <- rbind(a3,a4)
#   print(a1)
# }
# chi <- otherteam('CHI')



#### Pace ####
# pace <- function(dataset, gameID) {
#   dataset <- dataset %>%
#     filter(gameID == idGame)
#   d1 <- dataset[1,]
#   d2 <- dataset[2,]
#   print(48 * (((.5*((d1$fga + 0.4*d1$fta - 1.07*((d1$oreb)/(d1$oreb + d2$dreb))*(d1$fga-d1$fgm)+d1$tov) + 
#                       (d2$fga + 0.4*d2$fta - 1.07*((d2$oreb)/(d2$oreb + d1$dreb))*(d2$fga-d1$fgm)+d2$tov))) +
#                  (.5*((d2$fga + 0.4*d2$fta - 1.07*((d2$oreb)/(d2$oreb + d1$dreb))*(d2$fga-d2$fgm)+d2$tov) + 
#                         (d1$fga + 0.4*d1$fta - 1.07*((d1$oreb)/(d1$oreb + d2$dreb))*(d1$fga-d2$fgm)+d1$tov)))) / (2*minExact/5)))
#   
# }
# pace(TeamStats2021, 22100001)


# season_pace <- function(team) {
#   d3 <- otherteam(team)
#   d4 <- d3[2,]
#   d5 <- d3[1,]
#   d3 <- d3 %>%
#     mutate(pace = 48 * (((.5*((d4$fga + 0.4*d4$fta - 1.07*((d4$oreb)/(d4$oreb + d5$dreb))*(d4$fga-d4$fgm)+d4$tov) + 
#                                 (d5$fga + 0.4*d5$fta - 1.07*((d5$oreb)/(d5$oreb + d4$dreb))*(d5$fga-d4$fgm)+d5$tov))) +
#                            (.5*((d5$fga + 0.4*d5$fta - 1.07*((d5$oreb)/(d5$oreb + d4$dreb))*(d5$fga-d5$fgm)+d5$tov) + 
#                                   (d4$fga + 0.4*d4$fta - 1.07*((d4$oreb)/(d4$oreb + d5$dreb))*(d4$fga-d5$fgm)+d4$tov)))) / (2*minExact/5)))
#   d3 <- d3[1,9] 
#   print(d3)
# }
# chi <- season_pace('BKN')


