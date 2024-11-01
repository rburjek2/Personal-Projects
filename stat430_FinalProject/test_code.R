library(Lahman)


View(Pitching)

playerNames = as_tibble(People) %>%
  select(playerID, nameFirst, nameLast) %>%
  mutate(name = paste0(nameFirst, " ", nameLast)) %>%
  select(playerID, name)

batterStats = Batting %>%
  filter(yearID >= 1988 & yearID <= 2010) %>%
  filter(AB >= 100) %>%
  select(-stint, -lgID, -SB, -CS, -IBB, -SH, -GIDP) %>%
  mutate(OBP = round((H + BB + HBP)/(AB + BB + HBP + SF), 3),
         SLG = round((H + X2B + 2*X3B + 3*HR)/AB, 3),
         OPS = round(OBP + SLG, 3)) %>%
  left_join(playerNames, join_by(playerID)) %>%
  select(name, everything(), -playerID) %>%
  select(-HBP, -SF)

pitcherStats = Pitching %>%
  filter(yearID >= 1988 & yearID <= 2010) %>%
  filter(IPouts >= 60) %>%
  select(playerID, yearID, teamID, W, L, G, IPouts, H, ER, HR, BB, SO, ERA) %>%
  mutate(IP = round(IPouts/3, 1),
         WHIP = round(3*(H + BB)/IPouts, 3)) %>%
  left_join(playerNames, join_by(playerID)) %>%
  select(name, everything(), -playerID, -IPouts)

batStat = batterStats %>% pivot_longer(names_to = "stat",
                             cols = c("G", "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "BB", "SO", "OBP", "SLG", "OPS"))
batStat["pos"] = "b"
 
pitchStat  = pitcherStats %>% pivot_longer(names_to = "stat",
                                           cols = c("W", "L", "G", "H", "ER", "HR", "BB", "SO", "ERA", "IP", "WHIP"))
pitchStat["pos"] = "p"

playerStats = rbind(batStat, pitchStat)


playerStats %>%
  filter(yearID == 2010) %>%
  filter(pos == "b") %>%
  group_by(stat) %>%
  count() %>%
  ungroup() %>%
  select(stat) %>%
  as.vector()


playerStats %>%
  filter(yearID == 2010) %>%
  filter(teamID == "CHN") %>%
  filter(pos == "b") %>%
  filter(stat == "AB") %>%
  select(Name = name, Team = teamID, Total = value) %>%
  arrange(desc(Total))

View(pitcherStats %>%
  filter(yearID == 2010) %>%
  filter(teamID == "ARI"))


yearBatStats = playerStats %>%
  filter(pos == "b") %>%
  group_by(yearID, stat) %>%
  summarize(avg = mean(value, na.rm = TRUE))

yearBatStats$steroids = ifelse(yearBatStats$yearID >= 1994 & yearBatStats$yearID <= 2004, 1, 0)


yearPitchStats = playerStats %>%
  filter(pos == "p") %>%
  group_by(yearID, stat) %>%
  summarize(avg = mean(value, na.rm = TRUE))

yearPitchStats$steroids = ifelse(yearPitchStats$yearID >= 1994 & yearPitchStats$yearID <= 2004, 1, 0)

ggplot(data = yearPitchStats %>% filter(stat == "ERA"), aes(x = yearID, y = avg, color = steroids)) +
  geom_line()






playerStats %>%
  filter(pos == "b") %>%
  filter(stat == "HR") %>%
  group_by(yearID, stat) %>%
  summarise(avg = mean(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(steroids = ifelse(yearID >= 1994 & yearID <= 2004, 1, 0))
