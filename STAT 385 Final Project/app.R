#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("Lahman")

library(shiny)
library(Lahman)
library(tidyverse)
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
  filter(yearID >= 1985 & yearID <= max(yearly_salaries$yearID)) %>%
  select(yearID, teamID, lgID, divID, G, W, L, TeamName = name) %>%
  mutate(division = paste0(lgID, " ", divID)) %>%
  left_join(yearly_salaries, by = c("yearID", "teamID")) %>%
  mutate(WinPercentage = round(W/G, digits = 3), PayrollMillions = round(TeamPayroll/1000000, digits = 2))

# User Interface
ui = navbarPage(
  title = "MLB Yearly Team Payroll vs. Regular Season Win Percentage",
  tabPanel(
    title = "About",
    includeMarkdown("about.Rmd")),
  tabPanel(
    title = "Plot / Visualization",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "year",
                    label = "Year:",
                    choices = unique(yearly_team_payroll$yearID),
                    selected = 2016),
        selectInput(inputId = "team_view",
                    label = "View:",
                    choices = c("All", "National League", "American League"),
                    selected = "All"),
        checkboxInput(inputId = "color",
                      label = "Color by Division",
                      value = FALSE)
      ),
      mainPanel(
        plotOutput("win_perc_plot"),
        plotOutput("bar_graph")
      )
    )),
  tabPanel(
    title = "Table",
    dataTableOutput("table")
  )
)


server = function(input, output) {
  
    mlb_season = reactive({
      
      if (input$team_view == "National League") {
        yearly_team_payroll %>%
          filter(yearID == input$year) %>%
          calc_exp_effect() %>%
          filter(lgID == "NL")
      } else if (input$team_view == "American League") {
        yearly_team_payroll %>%
          filter(yearID == input$year) %>%
          calc_exp_effect() %>%
          filter(lgID == "AL")
      } else {
        yearly_team_payroll %>%
          filter(yearID == input$year) %>%
          calc_exp_effect()
      }
      
    })

    output$win_perc_plot = renderPlot({
      
      if (input$color == FALSE) {
        ggplot(data = mlb_season(), 
               aes(x = WinPercentage, y = PayrollMillions, color = input$year)) +
          geom_point() +
          scale_color_manual(values = "#ff0000",
                             name = "Year") +
          geom_hline(yintercept = mean(mlb_season()$PayrollMillions),
                     linetype = "dashed",
                     color = "black") +
          geom_vline(xintercept = mean(mlb_season()$WinPercentage),
                     linetype = "dashed",
                     color = "black") +
          geom_text(aes(label = teamID),
                    size = 3,
                    nudge_x = 0.008,
                    check_overlap = FALSE) +
          xlab("Win Percentage") +
          ylab("Team Payroll (in Millions)") +
          theme(aspect.ratio = 7/10)
      } else if (input$color == TRUE) {
        ggplot(data = mlb_season(), aes(x = WinPercentage,
                                        y = PayrollMillions,
                                        color = division)) +
          geom_point() +
          scale_color_manual(values = c("#ff8cc5", "#ff0000", "#bf0000",
                                       "#6db9ff", "#000cff", "#000569"),
                             name = "Division") +
          geom_hline(yintercept = mean(mlb_season()$PayrollMillions),
                     linetype = "dashed",
                     color = "black") +
          geom_vline(xintercept = mean(mlb_season()$WinPercentage),
                     linetype = "dashed",
                     color = "black") +
          geom_text(aes(label = teamID),
                    size = 3,
                    nudge_x = 0.008,
                    check_overlap = FALSE) +
          xlab("Win Percentage") +
          ylab("Team Payroll (in Millions)") +
          theme(aspect.ratio = 7/10)
      }
      
    })
    
    output$bar_graph = renderPlot({
      ggplot(data = mlb_season(),
             aes(x = reorder(teamID, -ExpEffectiveness), 
                 y = ExpEffectiveness)) +
        geom_col(aes(fill = ExpEffectiveness)) +
        scale_fill_gradient2(low = "#000cff", 
                             high = "#ff0000", 
                             midpoint = 0) +
        geom_text(aes(label = teamID),
                  size = 3,
                  color = "black",
                  fontface = "bold") +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        xlab("MLB Team") +
        ylab("Expenditure Effectiveness")
    })
    
    output$table = renderDataTable(
      mlb_season() %>%
        select(Year = yearID,
               "Team ID" = teamID,
               "Team Name" = TeamName,
               Division = division,
               Games = G,
               Wins = W,
               Losses = L,
               "Win %" = WinPercentage,
               "Team Payroll" = TeamPayroll,
               "Payroll (Millions)" = PayrollMillions,
               "Expenditure Effectiveness" = ExpEffectiveness)
    )
}

shinyApp(ui = ui, server = server)






















