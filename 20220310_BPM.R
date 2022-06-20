library(nbastatR)
library(dplyr)

#milbkn <- TeamStats2021 %>% filter(idGame == 22100001)


#### Total Possessions for a given team ####
tot_poss <- function(dataset, gameID, team) {
  dataset <- dataset %>%
    filter(gameID == idGame)
  d1 <- dataset %>%
    filter(slugTeam == team)
  d2 <- dataset %>%
    filter(slugTeam != team)
  print(.5*((d1$fga + 0.4*d1$fta - 1.07*((d1$oreb)/(d1$oreb + d2$dreb))*(d1$fga-d1$fgm)+d1$tov) + 
          (d2$fga + 0.4*d2$fta - 1.07*((d2$oreb)/(d2$oreb + d1$dreb))*(d2$fga-d1$fgm)+d2$tov)))
}
tot_poss(TeamStats2021, 22100001, 'MIL')

#### Points per 100 Possessions ####
pp100 <- function(dataset, gameID, team) {
  dataset <- dataset %>%
    filter(gameID == idGame)
  d1 <- dataset %>%
    filter(slugTeam == team)
  d2 <- dataset %>%
    filter(slugTeam != team)
  print((d1$pts/(.5*((d1$fga + 0.4*d1$fta - 1.07*((d1$oreb)/(d1$oreb + d2$dreb))*(d1$fga-d1$fgm)+d1$tov) + 
              (d2$fga + 0.4*d2$fta - 1.07*((d2$oreb)/(d2$oreb + d1$dreb))*(d2$fga-d1$fgm)+d2$tov))))*100)
}
pp100(TeamStats2021, 22100001, 'MIL')



#### Individual and Team Points per possession ####
#PTS /(FGA+(0.44 FTA)+TO)
pppp <- function(dataset, gameID, player) {
  d1 <- dataset %>%
    filter(gameID == idGame, player == namePlayer)
  print(d1$pts/(d1$fga + (.44*d1$fta) + d1$tov))
}
pppp(PlayerStats2021, 22100001, 'Kevin Durant')

tppp <- function(dataset, gameID, team) {
  d1 <- dataset %>%
    filter(gameID == idGame, team == slugTeam)
  print(d1$pts/(d1$fga + (.44*d1$fta) + d1$tov))
}
104/(tot_poss(TeamStats2021, 22100001, 'BKN'))


sg_bpm <- function(dataset, dataset2, gameID, team) {
  #filtering by gameID separating the two teams into two datasets
  #calculating pace based off possession calculation
  d3 <- dataset2 %>%
    filter(gameID == idGame)
  d4 <- d3[1,]
  d5 <- d3[2,]
  d3 <- d3 %>%
    mutate(pace = 48 * (((.5*((d4$fga + 0.4*d4$fta - 1.07*((d4$oreb)/(d4$oreb + d5$dreb))*(d4$fga-d4$fgm)+d4$tov) + 
                                (d5$fga + 0.4*d5$fta - 1.07*((d5$oreb)/(d5$oreb + d4$dreb))*(d5$fga-d4$fgm)+d5$tov))) +
                           (.5*((d5$fga + 0.4*d5$fta - 1.07*((d5$oreb)/(d5$oreb + d4$dreb))*(d5$fga-d5$fgm)+d5$tov) + 
                                  (d4$fga + 0.4*d4$fta - 1.07*((d4$oreb)/(d4$oreb + d5$dreb))*(d4$fga-d5$fgm)+d4$tov)))) / 96))
  #Filter by game and by team, adding total minutes, pts, fga, fta
  #adding team points/tsa by multiplying by the coefficients, adding baseline points for a constant, and game pace
  d2 <- dataset2 %>%
    filter(gameID == idGame, team == slugTeam) %>%
    select(minExact, pts, fga, fta) %>%
    mutate(tm_points_tsa = pts/(fga + (fta*(-.2464/-.56)))) %>%
    mutate(baseline_pts = 1) %>%
    mutate(pace = mean(d3$pace))
  d1 <- dataset %>%
    filter(gameID == idGame, team == slugTeam) %>%
    select(idGame, slugTeam, namePlayer, fgm, fga, pctFG, fg3m, fg3a, pctFG3,
           ftm, fta, pctFT, oreb, dreb, treb, ast, stl, blk, tov, pf, pts, 
           plusminus, minExact)
  d1 <- d1 %>%
    mutate(MP = minExact/(240/nrow(d1))) %>%
    mutate(TSA = d1$fga + (-.2464/-.56)*d1$fta)
  d1 <- d1 %>%
    mutate(Pts_TSA = d1$pts/d1$TSA) %>%
    mutate(Adj_pts = d1$pts + (d2$baseline_pts - d2$tm_points_tsa)*(d1$TSA)) %>%
    mutate(Possessions = (d1$minExact*d2$pace)/(d2$minExact/5)) 
  d1 <- d1 %>%
    mutate(Adj_pts100 = d1$Adj_pts/d1$Possessions*100) %>%
    mutate(fga100 = d1$fga/d1$Possessions*100) %>%
    mutate(fta100 = d1$fta/d1$Possessions*100) %>%
    mutate(ast100 = d1$ast/d1$Possessions*100) %>%
    mutate(oreb100 = d1$oreb/d1$Possessions*100) %>%
    mutate(dreb100 = d1$dreb/d1$Possessions*100) %>%
    mutate(treb100 = d1$treb/d1$Possessions*100) %>%
    mutate(tov100 = d1$tov/d1$Possessions*100) %>%
    mutate(stl100 = d1$stl/d1$Possessions*100) %>%
    mutate(blk100 = d1$blk/d1$Possessions*100) %>%
    mutate(pf100 = d1$pf/d1$Possessions*100)
  #mutate(min_per = d1$minExact/d2$MinExact/5)
  
  print(d1)
  #print(d2)
  #print(d3)
  #print(d4)
  #print(d5)
}

test <- sg_bpm(PlayerStats2021, TeamStats2021, 22100001, 'MIL')




