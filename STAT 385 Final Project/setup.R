
library(Lahman)
library(dplyr)
library(ggplot2)

yearly_salaries = as_tibble(Salaries) %>%
  left_join(as_tibble(People) %>% select(playerID, nameFirst, nameLast), by = "playerID") %>%
  mutate(Name = paste0(nameLast, ", ", nameFirst)) %>%
  group_by(yearID, teamID) %>%
  mutate(TeamPayroll = sum(salary)) %>%
  ungroup() %>%
  group_by(yearID, teamID, TeamPayroll) %>%
  summarize()

yearly_team_payroll = as_tibble(Teams) %>%
  filter(yearID >= 1985 & yearID <= 2016) %>%
  select(yearID, teamID, lgID, divID, G, W, L, TeamName = name) %>%
  left_join(yearly_salaries, by = c("yearID", "teamID")) %>%
  mutate(WinPercentage = round(W/G, digits = 3))

yearly_team_payroll %>%
  filter(yearID == 2002) %>%
  mutate(PayrollMillions = round(TeamPayroll/1000000, digits = 1)) %>%
  ggplot(aes(x = WinPercentage, y = PayrollMillions)) +
    geom_point() +
    geom_hline(mean(PayrollMillions)) +
    xlab("Win Percentage") +
    ylab("Team Payroll (in Millions)")

payroll_2002 = yearly_team_payroll %>%
  filter(yearID == 2002) %>%
  mutate(PayrollMillions = round(TeamPayroll/1000000, digits = 2)) %>%
  mutate(WinPercDiff = round((WinPercentage*100)-(mean(WinPercentage*100)), digits = 1)) %>%
  mutate(PayrollWeight = (PayrollMillions-min(PayrollMillions))/100)

View(payroll_2002)

ggplot(data = payroll_2002, aes(x = WinPercentage, y = PayrollMillions, color=lgID)) +
  geom_point() +
  scale_color_manual(values = c("#ff0000", "#000cff")) +
  geom_hline(yintercept = mean(payroll_2002$PayrollMillions),
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = mean(payroll_2002$WinPercentage),
             linetype = "dashed",
             color = "black") +
  xlab("Win Percentage") +
  ylab("Team Payroll (in Millions)")


payroll_2016 = yearly_team_payroll %>%
  filter(yearID == 2016) %>%
  mutate(PayrollMillions = round(TeamPayroll/1000000, digits = 2)) %>%
  mutate(WinPercDiff = (WinPercentage*100)-(mean(WinPercentage*100))) %>%
  mutate(PayrollWeight = (PayrollMillions-mean(PayrollMillions))/100) %>%
  mutate(ExpEffectiveness = WinPercDiff - PayrollWeight)

View(payroll_2016)

ggplot(data = payroll_2016, aes(x = WinPercentage, y = PayrollMillions, color=lgID)) +
  geom_point() +
  scale_color_manual(values = c("#ff0000", "#000cff")) +
  geom_hline(yintercept = mean(payroll_2016$PayrollMillions),
             linetype = "dashed",
             color = "black") +
  geom_vline(xintercept = mean(payroll_2016$WinPercentage),
             linetype = "dashed",
             color = "black") +
  geom_text(aes(label = teamID),
            size = 3,
            nudge_x = 0.0085,
            check_overlap = FALSE) +
  xlab("Win Percentage") +
  ylab("Team Payroll (in Millions)")

ggplot(data = payroll_2016,
       aes(x = reorder(teamID, -ExpEffectiveness), 
           y = ExpEffectiveness)) +
  geom_col(aes(fill = ExpEffectiveness)) +
  scale_fill_gradient2(low = "#000cff", 
                       high = "#ff0000", 
                       midpoint = 0) +
  geom_text(aes(label = teamID),
            size = 2.5,
            color = "black") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
  xlab("MLB Team") +
  ylab("Expenditure Effectiveness")


View( payroll_2016 %>%
  arrange(desc(ExpEffectiveness)) )


mean(payroll_2002$WinPercentage)


yearly_team_payroll = as_tibble(Teams) %>%
  filter(yearID >= 1985 & yearID <= 2016) %>%
  select(yearID, teamID, lgID, divID, G, W, L, TeamName = name) %>%
  mutate(division = paste0(lgID, " ", divID)) %>%
  left_join(yearly_salaries, by = c("yearID", "teamID")) %>%
  mutate(WinPercentage = round(W/G, digits = 3), PayrollMillions = TeamPayroll/1000000)

View(yearly_team_payroll)






