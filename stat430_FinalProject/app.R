#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

authorNames = "Jonah Winter, Jace Rice, Lily Olson, RJ Burjek"

library(shiny)
library(Lahman)
library(tidyverse)
library(ggplot2)

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
batStat["pos"] = "Batters"

pitchStat  = pitcherStats %>% pivot_longer(names_to = "stat",
                                           cols = c("W", "L", "G", "H", "ER", "HR", "BB", "SO", "ERA", "IP", "WHIP"))
pitchStat["pos"] = "Pitchers"

playerStats = rbind(batStat, pitchStat)

playerStats$steroids = ifelse(playerStats$yearID >= 1994 & playerStats$yearID <= 2004, 1, 0)



ui <- navbarPage(
  
  title = "MLB Player Statistics from 1988-2010",
  
  tabPanel(
    title = "About",
    includeMarkdown("about_page.Rmd")
  ),

  tabPanel(
    title = "Table",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "year",
                    label = "Year:",
                    choices = unique(playerStats$yearID),
                    selected = 2010),
        selectInput(inputId = "team",
                    label = "Team:",
                    choices = unique(playerStats$teamID)),
        radioButtons(inputId = "pos",
                     label = "Position:",
                     choices = unique(playerStats$pos)),
        selectInput(inputId = "stat",
                    label = "Stat:",
                    choices = unique(playerStats$stat))
      ),
      
      mainPanel(
        title = "Table",
        dataTableOutput("table"),
        plotOutput("bar_graph")
      )
    )
  ),
  
  tabPanel(
    title = "Steroid Era",
    sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "s_pos",
                     label = "Position:",
                     choices = unique(playerStats$pos)),
        selectInput(inputId = "s_stat",
                    label = "Stat:",
                    choices = unique(playerStats$stat))
        
      ),
      
      mainPanel(
        title = "Steroid Era",
        plotOutput("line_graph")
      )
    )
  )
  
)


server <- function(input, output, session) {
  
  teamPlayers = reactive({

    playerStats %>%
      filter(yearID == input$year) %>%
      filter(teamID == input$team) %>%
      filter(pos == input$pos) %>%
      filter(stat == input$stat) %>%
      select(Name = name, Team = teamID, Total = value) %>%
      arrange(desc(Total))
    
  })
  
  teamNames = reactive({
    
      batterStats %>%
        filter(yearID == input$year) %>%
        select(teamID) %>%
        group_by(teamID) %>%
        count() %>%
        ungroup() %>%
        select(teamID) %>%
        as.vector()
    
  })
  
  statNames = reactive({
    
    playerStats %>%
      filter(yearID == input$year) %>%
      filter(pos == input$pos) %>%
      group_by(stat) %>%
      count() %>%
      ungroup() %>%
      select(stat) %>%
      as.vector()
    
  })
  
  yearlyTeamStats = reactive({
    
    playerStats %>%
      filter(pos == input$s_pos) %>%
      group_by(yearID, stat) %>%
      summarise(avg = mean(value, na.rm = TRUE)) %>%
      ungroup()
    
  })
  
  yearStatNames = reactive({
    
    playerStats %>%
      filter(pos == input$s_pos) %>%
      group_by(yearID, stat) %>%
      summarise(avg = mean(value, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(stat) %>%
      count() %>%
      ungroup() %>%
      select(stat) %>%
      as.vector()
    
  })
  
  graphStat = reactive({
    
    playerStats %>%
      filter(pos == input$s_pos) %>%
      filter(stat == input$s_stat) %>%
      group_by(yearID, stat) %>%
      summarise(avg = mean(value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(steroids = ifelse(yearID >= 1994 & yearID <= 2004, 1, 0))
    
  })
  
  observe({
    updateSelectInput(session,
                      "team",
                      choices = teamNames())
  })
  
  observe({
    updateSelectInput(session,
                      "stat",
                      choices = statNames())
  })
  
  observe({
    updateSelectInput(session,
                      "s_stat",
                      choices = yearStatNames())
  })

  
  output$bar_graph = renderPlot({
    ggplot(data = teamPlayers(),
           aes(x = reorder(Name, Total), 
               y = Total)) +
      geom_col(aes(fill = Total)) +
      scale_fill_gradient2(low = "#0009b5", 
                           high = "#74ccff") +
      coord_flip() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      xlab("Player") +
      ylab("Total")
  })
  
  output$line_graph = renderPlot({
  
    ggplot(data = graphStat(), 
          aes(x = yearID, y = avg, color = steroids)) +
      geom_line() +
      xlab("Year") +
      ylab("Average Value") +
      labs(title = "League Average from 1988-2010")
  
  })
  
  output$table = renderDataTable(
    teamPlayers() %>% head(10)
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
