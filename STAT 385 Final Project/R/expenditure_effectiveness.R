library(tidyverse)

calc_exp_effect = function(data) {
  data %>%
    mutate(WinPercDiff = (WinPercentage*100)-(mean(WinPercentage*100))) %>%
    mutate(PayrollWeight = (PayrollMillions-mean(PayrollMillions))/100) %>%
    mutate(ExpEffectiveness = round(WinPercDiff - PayrollWeight, digits = 2))
}
