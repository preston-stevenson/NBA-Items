library(dplyr)


PlayersAggregate21 <- PlayerStats2021 %>%
  select(!c(idTeam, fg2m, fg2a, pctFG2)) %>%
  group_by(namePlayer, slugTeam) %>%
  summarize(pts = mean(pts), asts = mean(ast), reb = mean(treb), stl = mean(stl), blk = mean(blk),
            plusminus = mean(plusminus), pctFG = mean(pctFG), pct3FG = mean(pctFG3), mins = mean(minExact))

TeamsAggregate21 <- TeamStats2021 %>%
  select(!c(idTeam, fg2m, fg2a, pctFG2)) %>%
  group_by(slugTeam) %>%
  summarize(pts = mean(pts), asts = mean(ast), reb = mean(treb), stl = mean(stl), blk = mean(blk),
            plusminus = mean(plusminus), pctFG = mean(pctFG), pct3FG = mean(pctFG3),
            pctFGContested = mean(pctFGContested), fgaContested = mean(fgaContested),
            pctFGUncontested = mean(pctFGUncontested), fgaUncontested = mean(fgaUncontested),
            pctFGRimDefended = mean(pctFGRimDefended), fgaRimDefended = mean(fgaRimDefended),
            touches = mean(touches), astSecondary = mean(astSecondary), ftAst = mean(ftAST),
            miles = mean(distMiles), pctRebchance = mean(sum(treb)/sum(trebChances)), mins = mean(minExact))


# UtahAdvanced21 <- TeamsAdvanced21 %>%
#   filter(slugTeam == 'UTA') %>%
#   select(namePlayer, pctFGContested, fgaContested, pctFGRimDefended, fgaRimDefended,
#          pctFGUncontested, fgaUncontested, pts, asts) %>%
#   arrange(desc(pctFGContested))


# test <- PlayerStats2021 %>%
#   select(namePlayer, treb, trebChances) %>%
#   filter(namePlayer == 'Ade Murkey')
# 
# test2 <- TeamStats2021 %>%
#   #select(namePlayer, treb, trebChances) %>%
#   #filter(slugTeam == 'BKN', namePlayer == 'Andre Drummond')
#   filter(idGame %in% (22100450:22100460))
