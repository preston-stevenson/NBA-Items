library(nbastatR)
library(dplyr)

s_bpm_tab <- function(team) {
  #filter for team and then get totals for min, pts, fga, etc
  d1 <- TeamStats2021 %>%
    filter(team == slugTeam) %>%
    mutate(G = 1) %>%
    summarize(minExact = sum(minExact), pts = sum(pts),
              fga = sum(fga), fta = sum(fta), pace = mean(pace), 
              ortg = mean(ortg), drtg = mean(drtg), netrtg = mean(netrtg),
              treb = sum(treb), stl = sum(stl), pf = sum(pf), blk = sum(blk),
              ast = sum(ast), ortg = mean(ortg), drtg = mean(drtg))
  #add tm points/tsa and then baseline points
  d1 <- d1 %>%
    mutate(tm_points_tsa = pts/(fga + (fta*(-.2464/-.56)))) %>%
    mutate(baseline_pts = 1) 
  # new dataset to look at individual players
  #filter by team, add games so it will count total games per player
  #group by player and team then sum up all the stats except for the percentages
  d2 <- PlayerStats2021 %>%
    filter(team == slugTeam) %>%
    mutate(G = 1) %>%
    group_by(namePlayer, slugTeam) %>%
    summarize(G = sum(G), pts = sum(pts), ast = sum(ast), stl = sum(stl), blk = sum(blk),
          plusminus = sum(plusminus), pctFG = mean(pctFG), pct3FG = mean(pctFG3), MP = sum(minExact),
          fga = sum(fga), fta = sum(fta), fgm = sum(fgm), ftm = sum(ftm), pctFT = mean(pctFT), fg3m = sum(fg3m),
          fg2a = sum(fg2a), fg2m = sum(fg2m), pctFG2 = mean(pctFG2), dreb = sum(dreb), oreb = sum(oreb),
          treb = sum(treb), tov = sum(tov), pf = sum(pf), pos_val = mean(pos_val))
 #start adding advanced stats, they use the other fields so it is broken up
  d2 <- d2 %>%
    mutate(efg = (fgm + .5*fg3m)/fga) %>%
    mutate(tsa = fga + (.44*fta)) %>%
    mutate(pts_tsa = pts/tsa)
  #same case here
  d2 <- d2 %>%
    mutate(adj_pts = (pts_tsa-d1$tm_points_tsa+1)*tsa) %>%
    mutate(poss = (MP*d1$pace)/48)
  d2$adj_pts[is.na(d2$adj_pts)] <- 0
  #same thing, adding all the per 100 stats now
  d2 <- d2 %>%
    mutate(adj_pts100 = adj_pts/poss*100) %>%
    mutate(fga100 = fga/poss*100) %>%
    mutate(fta100 = fta/poss*100) %>%
    mutate(fg3m100 = fg3m/poss*100) %>%
    mutate(ast100 = ast/poss*100) %>%
    mutate(tov100 = tov/poss*100) %>%
    mutate(oreb100 = oreb/poss*100) %>%
    mutate(dreb100 = dreb/poss*100) %>%
    mutate(treb100 = treb/poss*100) %>%
    mutate(stl100 = stl/poss*100) %>%
    mutate(blk100 = blk/poss*100) %>%
    mutate(pf100 = pf/poss*100) 
  d2 <- d2 %>%
    mutate(thresh_pts = tsa * (pts_tsa-(d1$tm_points_tsa+off_role_pos$pt_thresh))) %>%
    mutate(pct_MP = MP/(d1$minExact/5))
  d2$thresh_pts[is.na(d2$thresh_pts)] <- 0
  
  #after adding threshold points to d2 we need team threshold points in d1
  d1 <- d1 %>%
    mutate(tm_thresh_pts = sum(d2$thresh_pts, na.rm = TRUE))
  
  #adding pct of stats for when player is on the floor
  d2 <- d2 %>%
    mutate(pct_stl = stl/d1$stl/pct_MP) %>%
    mutate(pct_treb = treb/d1$treb/pct_MP) %>%
    mutate(pct_blk = blk/d1$blk/pct_MP) %>%
    mutate(pct_ast = ast/d1$ast/pct_MP) %>%
    mutate(pct_pf = pf/d1$pf/pct_MP) %>%
    mutate(pct_thresh_pts = thresh_pts/d1$tm_thresh_pts/pct_MP)
  
  #calculating estimated position using average position stat table against their pct stats
  d2 <- d2 %>%
    mutate(est_pos_1 = pos_per[1,1] + (pct_treb*pos_per$pct_TRB) + (pct_stl*pos_per$pct_STL)
                                    + (pct_blk*pos_per$pct_BLK) + (pct_ast*pos_per$pct_AST)
                                    + (pct_pf*pos_per$pct_PF))
  #starting wild section of adding all of the adjusted positions until the team avg rounds to 3
  #round 1
  d2 <- d2 %>%
    mutate(min_adj_1 = ((est_pos_1*MP) + (pos_val*pos_per[1,7]))/ (MP+pos_per[1,7]))
  d2 <- d2 %>%
    mutate(trim_1 = max(min(min_adj_1, 5), 1))
  d2 <- d2 %>%
    mutate(tm_avg_top1 = trim_1*MP)
  d1 <- d1 %>%
    mutate(tm_avg_1 = sum(d2$tm_avg_top1)/minExact)
  #round 2 
  d2 <- d2 %>%
    mutate(adj_pos_2 = min_adj_1 - (d1$tm_avg_1 - 3))
  d2 <- d2 %>%
    mutate(trim_2 = max(min(adj_pos_2, 5), 1))
  d2 <- d2 %>%
    mutate(tm_avg_top2 = trim_2*MP)
  d1 <- d1 %>%
    mutate(tm_avg_2 = sum(d2$tm_avg_top2)/minExact)
  #round 3
  d2 <- d2 %>%
    mutate(adj_pos_3 = min_adj_1 - (d1$tm_avg_1 - 3) - (d1$tm_avg_2 - 3))
  d2 <- d2 %>%
    mutate(trim_3 = max(min(adj_pos_3, 5), 1))
  d2 <- d2 %>%
    mutate(tm_avg_top3 = trim_3*MP)
  d1 <- d1 %>%
    mutate(tm_avg_3 = sum(d2$tm_avg_top3)/minExact)
  #round 4
  d2 <- d2 %>%
    mutate(adj_pos_4 = min_adj_1 - (d1$tm_avg_1 - 3) - (d1$tm_avg_2 - 3) - (d1$tm_avg_3 - 3))
  d2 <- d2 %>%
    mutate(trim_4 = max(min(adj_pos_4, 5), 1))
  d2 <- d2 %>%
    mutate(tm_avg_top4 = trim_4*MP)
  d1 <- d1 %>%
    mutate(tm_avg_4 = sum(d2$tm_avg_top4)/minExact)
  #finishing and calculating on the round 5
  d2 <- d2 %>%
    mutate(adj_pos_5 = min_adj_1 - (d1$tm_avg_1 - 3) - (d1$tm_avg_2 - 3) 
           - (d1$tm_avg_3 - 3) - (d1$tm_avg_4 - 3))
  d2 <- d2 %>%
    mutate(position_adj = max(min(adj_pos_5, 5), 1))
  
  #starting similar process but calculating offensive position value
  d2 <- d2 %>%
    mutate(est_off_role = off_role_pos$intercept + (off_role_pos$pct_ast*pct_ast) 
           + (off_role_pos$pct_thresh_points*pct_thresh_pts))
  d2$est_off_role[is.na(d2$est_off_role)] <- 0
  d2 <- d2 %>%
    mutate(o_min_adj_1 = ((est_off_role*MP) + (off_role_pos$Default_pos*off_role_pos$Min_Wt))/
             (MP + off_role_pos$Min_Wt))
  d2 <- d2 %>%
    mutate(o_trim_1 = max(min(o_min_adj_1, 5), 1))
  d2 <- d2 %>%
    mutate(o_tm_avg_top1 = o_min_adj_1*MP)
  #removing NA's
  d2$o_tm_avg_top1[is.na(d2$o_tm_avg_top1)] <- 0
  d1 <- d1 %>%
    mutate(o_tm_avg_1 = sum(d2$o_tm_avg_top1)/minExact)
  #round 2
  d2 <- d2 %>%
    mutate(o_adj_2 = (o_min_adj_1 - (d1$o_tm_avg_1 - 3)))
  d2 <- d2 %>%
    mutate(o_trim_2 = max(min(o_adj_2, 5), 1))
  d2 <- d2 %>%
    mutate(o_tm_avg_top2 = o_adj_2*MP)
  d2$o_tm_avg_top2[is.na(d2$o_tm_avg_top2)] <- 0
  d1 <- d1 %>%
    mutate(o_tm_avg_2 = sum(d2$o_tm_avg_top2)/minExact)
  #round 3
  d2 <- d2 %>%
    mutate(o_adj_3 = (o_min_adj_1 - (d1$o_tm_avg_1 - 3) - (d1$o_tm_avg_2 - 3)))
  d2 <- d2 %>%
    mutate(o_trim_3 = max(min(o_adj_3, 5), 1))
  d2 <- d2 %>%
    mutate(o_tm_avg_top3 = o_adj_3*MP)
  d2$o_tm_avg_top3[is.na(d2$o_tm_avg_top3)] <- 0
  d1 <- d1 %>%
    mutate(o_tm_avg_3 = sum(d2$o_tm_avg_top3)/minExact)
  #round 4 is used as the official numbers, once the tm avg gets to 3 for all teams
  d2 <- d2 %>%
    mutate(o_adj_4 = (o_min_adj_1 - (d1$o_tm_avg_1 - 3) - (d1$o_tm_avg_2 - 3) - (d1$o_tm_avg_3 - 3)))
  d2 <- d2 %>%
    mutate(o_position_adj = max(min(o_adj_4, 5), 1))
  #BPM coefficients to use based on positions
  d2 <- d2 %>%
    mutate(co_adj_pts = ((5 - position_adj)/4*co_bpm[1,1]) 
           + ((position_adj - 1)/4*co_bpm[2,1])) %>%
    mutate(co_fga = ((5 - o_position_adj)/4*co_bpm[1,2]) 
           + ((o_position_adj - 1)/4*co_bpm[2,2])) %>%
    mutate(co_fta = ((5 - o_position_adj)/4*co_bpm[1,3]) 
           + ((o_position_adj - 1)/4*co_bpm[2,3])) %>%
    mutate(co_fg3m = ((5 - position_adj)/4*co_bpm[1,4]) 
           + ((position_adj - 1)/4*co_bpm[2,4])) %>%
    mutate(co_ast = ((5 - position_adj)/4*co_bpm[1,5]) 
           + ((position_adj - 1)/4*co_bpm[2,5])) %>%
    mutate(co_tov = ((5 - position_adj)/4*co_bpm[1,6]) 
           + ((position_adj - 1)/4*co_bpm[2,6])) %>%
    mutate(co_oreb = ((5 - position_adj)/4*co_bpm[1,7]) 
           + ((position_adj - 1)/4*co_bpm[2,7])) %>%
    mutate(co_dreb = ((5 - position_adj)/4*co_bpm[1,8]) 
           + ((position_adj - 1)/4*co_bpm[2,8])) %>%
    mutate(co_treb = ((5 - position_adj)/4*co_bpm[1,9]) 
           + ((position_adj - 1)/4*co_bpm[2,9])) %>%
    mutate(co_stl = ((5 - position_adj)/4*co_bpm[1,10]) 
           + ((position_adj - 1)/4*co_bpm[2,10])) %>%
    mutate(co_blk = ((5 - position_adj)/4*co_bpm[1,11]) 
           + ((position_adj - 1)/4*co_bpm[2,11])) %>%
    mutate(co_pf = ((5 - position_adj)/4*co_bpm[1,12]) 
           + ((position_adj - 1)/4*co_bpm[2,12]))
  
  #raw bpm calculation for different categories
  d2 <- d2 %>%
    mutate(scoring = (co_adj_pts*adj_pts100) + (co_fga*fga100) 
           + (co_fta*fta100) + (co_fg3m*fg3m100)) %>%
    mutate(ballhandling = (co_ast*ast100) + (co_tov*tov100)) %>%
    mutate(rebounding = (co_oreb*oreb100) + (co_dreb*dreb100) + (co_treb*treb100)) %>%
    mutate(defense = (co_stl*stl100) + (co_blk*blk100) + (co_pf*pf100))
  
  d2 <- d2 %>%
    mutate(pos_constant = if(position_adj < 3){
      (3 - position_adj)/2*-.818
    } else{
      1.387*(o_position_adj-3)
    }) 
  d2 <- d2 %>%
    mutate(rawbpm = scoring + ballhandling + rebounding + defense + pos_constant)
  d2 <- d2 %>%
    mutate(o_co_adj_pts = ((5 - position_adj)/4*co_obpm[1,1]) 
           + ((position_adj - 1)/4*co_obpm[2,1])) %>%
    mutate(o_co_fga = ((5 - o_position_adj)/4*co_obpm[1,2]) 
           + ((o_position_adj - 1)/4*co_obpm[2,2])) %>%
    mutate(o_co_fta = ((5 - o_position_adj)/4*co_obpm[1,3]) 
           + ((o_position_adj - 1)/4*co_obpm[2,3])) %>%
    mutate(o_co_fg3m = ((5 - position_adj)/4*co_obpm[1,4]) 
           + ((position_adj - 1)/4*co_obpm[2,4])) %>%
    mutate(o_co_ast = ((5 - position_adj)/4*co_obpm[1,5]) 
           + ((position_adj - 1)/4*co_obpm[2,5])) %>%
    mutate(o_co_tov = ((5 - position_adj)/4*co_obpm[1,6]) 
           + ((position_adj - 1)/4*co_obpm[2,6])) %>%
    mutate(o_co_oreb = ((5 - position_adj)/4*co_obpm[1,7]) 
           + ((position_adj - 1)/4*co_obpm[2,7])) %>%
    mutate(o_co_dreb = ((5 - position_adj)/4*co_obpm[1,8]) 
           + ((position_adj - 1)/4*co_obpm[2,8])) %>%
    mutate(o_co_treb = ((5 - position_adj)/4*co_obpm[1,9]) 
           + ((position_adj - 1)/4*co_obpm[2,9])) %>%
    mutate(o_co_stl = ((5 - position_adj)/4*co_obpm[1,10]) 
           + ((position_adj - 1)/4*co_obpm[2,10])) %>%
    mutate(o_co_blk = ((5 - position_adj)/4*co_obpm[1,11]) 
           + ((position_adj - 1)/4*co_obpm[2,11])) %>%
    mutate(o_co_pf = ((5 - position_adj)/4*co_obpm[1,12]) 
           + ((position_adj - 1)/4*co_obpm[2,12]))
  d2 <- d2 %>%
    mutate(o_scoring = (o_co_adj_pts*adj_pts100) + (o_co_fga*fga100) 
           + (o_co_fta*fta100) + (o_co_fg3m*fg3m100)) %>%
    mutate(o_ballhandling = (o_co_ast*ast100) + (o_co_tov*tov100)) %>%
    mutate(o_rebounding = (o_co_oreb*oreb100) + (o_co_dreb*dreb100) + (o_co_treb*treb100)) %>%
    mutate(o_defense = (o_co_stl*stl100) + (o_co_blk*blk100) + (o_co_pf*pf100))
  d2 <- d2 %>%
    mutate(o_pos_constant = if(position_adj < 3){
      (3 - position_adj)/2*-1.698
    } else{
      .43*(o_position_adj-3)
    }) 
  d2 <- d2 %>%
    mutate(rawobpm = o_scoring + o_ballhandling + o_rebounding + o_defense + o_pos_constant)
  d2 <- d2 %>%
    mutate(rawbpm_cont = rawbpm*pct_MP) %>%
    mutate(rawobpm_cont = rawobpm*pct_MP) %>%
    mutate(MPG = MP/G) %>%
    mutate(reMPG = MP/(G+4)) %>%
    mutate(reMin = max((450-MP)/3, 0)) %>%
    mutate(expBPM = 4.75 + .175*reMPG)
 
  print(d2)
}

test3 <- s_bpm_tab('SAC')

teamrating <- function(team){
  
  d0 <- s_bpm_tab(team)
  #This is used to calculate the NBA's overall ortg
  d1 <- TeamStats2021 %>%
    summarize(rtg = mean(ortg))
  
  d2 <- TeamStats2021 %>%
    filter(slugTeam == team) %>%
    mutate(G = 1) %>%
    summarize(tortg = mean(ortg), tdrtg = mean(drtg), pace = mean(pace), G = sum(G))
  
  d2 <- d2 %>%
    mutate(ortg = tortg-d1$rtg) %>%
    mutate(drtg = d1$rtg-tdrtg) %>%
    select(ortg, drtg, pace, G) %>%
    mutate(Team_rtg = ortg + drtg) %>%
    mutate(avg_lead = Team_rtg*pace/100/2) %>%
    mutate(lead_bonus = .35/2*avg_lead) %>%
    mutate(adj_tm_rtg = Team_rtg + lead_bonus) %>%
    mutate(adj_o_rtg = ortg + lead_bonus/2)
  d3 <- d0 %>%
    select(namePlayer, rawbpm_cont, rawobpm_cont)
  d3 <- d3[,2:3]
  d3 <- d3 %>%
    summarize(rawbpm_cont = sum(rawbpm_cont), rawobpm_cont = sum(rawobpm_cont))
  d3 <- d3 %>%
    mutate(adj_rating = (d2$adj_tm_rtg - rawbpm_cont)/5) %>%
    mutate(adj_orating = (d2$adj_o_rtg - rawobpm_cont)/5)
  d2 <- cbind(d2,d3)
  print(d2)
}
ask <- teamrating('MIL')

s_bpm <- function(team) {
  
  d1 <- s_bpm_tab(team)
  d2 <- teamrating(team)
  d1 <- d1 %>%
    mutate(BPM = rawbpm + d2$adj_rating) %>%
    mutate(OBPM = rawobpm + d2$adj_orating) %>%
    mutate(DBPM = BPM - OBPM) %>%
    mutate(Contribution = BPM*pct_MP) %>%
    mutate(VORP = (BPM + 2) * pct_MP*d2$G/82) %>%
    mutate(reBPM = (BPM*MP + reMin*expBPM)/(MP+reMin)) %>%
    mutate(reOBPM = (OBPM*MP + reMin*expBPM)/(MP+reMin)) %>%
    mutate(reDBPM = reBPM - reOBPM) %>%
    select(namePlayer, slugTeam, position_adj, o_position_adj, MP, MPG, BPM, OBPM, DBPM, Contribution,
           VORP, reMPG, reMin, expBPM, reBPM, reOBPM, reDBPM) %>%
    arrange(desc(BPM))
  print(d1)
}
asks <- s_bpm('PHX')

all_bpm <- function(){
  output = data.frame()
  for (i in teamlist) {
   
    result <- s_bpm(i)
    output <- rbind(output, result)
  }
  print(output)
}
test <- all_bpm()

test1 <- test %>%
  #filter(namePlayer == 'James Harden') #%>%
  arrange(desc(MP))

