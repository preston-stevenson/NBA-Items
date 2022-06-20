library(dplyr)
library(nbastatR)

df_salaries <- hoopshype_salaries(all_teams = TRUE,
                     nest_data = F, return_message = T)
df_salaries <- df_salaries %>%
  arrange(namePlayer)
df_salaries[df_salaries == "Ja'Vonte Smart"] <- 'Javonte Smart'

ques <- df_salaries %>%
  filter(namePlayer == "Ja'Vonte Smart")

df_salaries_c <- df_salaries %>%
  filter(slugSeason == '2021-22') %>%
  select(namePlayer, slugSeason, amountContract, isFinalSeason) %>%
  group_by(namePlayer) %>%
  summarize(amountContract = sum(amountContract))

question <- asks %>%
  select(namePlayer, slugTeam, reBPM, reOBPM, position_adj, MP) 

question1 <- merge(x=question, y=df_salaries_c, by = "namePlayer", all.x = TRUE) %>%
  arrange(desc(amountContract))

question1 <- question1 %>%
  mutate(Ratio = amountContract/reBPM)
