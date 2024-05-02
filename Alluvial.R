# Libraries 
library(tidyverse)
library(worldfootballR)
library(ggalluvial)
library(extrafont)
library(ggtext)
library(MetBrewer)

alluvial_ui <- fluidPage(
      tags$style(HTML("
    body {
      background-color: black;
      color: white;
    }
    .well {
      background-color: black;
    }
    .selectize-input {
      color: black;
      background-color: black;
    }
    .selectize-dropdown {
      color: white;
      background-color: black;
    }
    .main-header {
      text-align: center;
    }
    .container-fluid {
      background-color: black;
    }
    .shiny-plot-output {
      background-color: black;
    }
  ")),

      titlePanel("Shot Flows by Team and Result"),
  fluidRow(
    column(width = 3, 
           wellPanel(
             selectInput("alluvial_season", "Select Season Year:",
                         choices = c("2020", "2021"),
                         selected = "2020"),
             selectInput("alluvial_league", "Select League:",
                         choices = c("EPL", "La liga", "Serie A", "Bundesliga", "Ligue 1"),
                         selected = "EPL")
           )
    ),
    column(width = 9, 
           plotOutput("alluvialPlot", height = "700px", width = "100%")
    )
  )
)

# Define server logic
alluvial_server <- function(input, output) {
  
  data <- reactive({
    file_path <- paste0("data/season_shots/", input$alluvial_league, "_", input$alluvial_season, "_shots.csv")
    data <- read.csv(file_path)
    return(data)
  })

  output$alluvialPlot <- renderPlot({
    req(data()) 
    data <- data() %>%
      mutate(isGoal = ifelse(result == "Goal", "Goal", "No Goal"))

    data1 <- data %>%
      filter(h_a == "h") %>%
      rename(Squad = home_team) %>%
      filter(case_when(
        input$alluvial_league == "Ligue 1" ~ Squad %in% c("Lens", "Lille", "Lyon", "Marseille","Monaco","Paris SG"),
        input$alluvial_league == "EPL" ~ Squad %in% c("Manchester United", "Chelsea", "Liverpool", "Manchester City", "Tottenham", "Arsenal"),
        input$alluvial_league == "La liga" ~ Squad %in% c("Barcelona", "Real Madrid", "Atletico Madrid", "Sevilla", "Real Sociedad", "Villarreal"),
        input$alluvial_league == "Serie A" ~ Squad %in% c("Atalanta", "Inter", "Juventus", "Milan", "Napoli", "Roma"),
        input$alluvial_league == "Bundesliga" ~ Squad %in% c("Bayern Munich", "Borussia Dortmund", "RB Leipzig", "Bayer Leverkusen", "Wolfsburg", "Eintracht Frankfurt"),
        TRUE ~ TRUE
      )) %>%
      select(Squad, isGoal, situation, xG)

    data2 <- data %>%
      filter(h_a == "a") %>%
      rename(Squad = away_team) %>%
      filter(case_when(
        input$alluvial_league == "Ligue 1" ~ Squad %in% c("Lens", "Lille", "Lyon", "Marseille","Monaco","Paris SG"),
        input$alluvial_league == "EPL" ~ Squad %in% c("Manchester United", "Chelsea", "Liverpool", "Manchester City", "Tottenham", "Arsenal"),
        input$alluvial_league == "La liga" ~ Squad %in% c("Barcelona", "Real Madrid", "Atletico Madrid", "Sevilla", "Real Sociedad", "Villarreal"),
        input$alluvial_league == "Serie A" ~ Squad %in% c("Atalanta", "Inter", "Juventus", "Milan", "Napoli", "Roma"),
        input$alluvial_league == "Bundesliga" ~ Squad %in% c("Bayern Munich", "Borussia Dortmund", "RB Leipzig", "Bayer Leverkusen", "Wolfsburg", "Eintracht Frankfurt"),
        TRUE ~ TRUE
      )) %>%
      select(Squad, isGoal, situation, xG)

    data <- rbind(data1, data2)

    data$Squad <- case_when(
      input$alluvial_league == "EPL" & data$Squad == "Manchester United" ~ "Man Utd",
      input$alluvial_league == "EPL" & data$Squad == "Manchester City" ~ "Man City",
      input$alluvial_league == "La liga" & data$Squad == "Barcelona" ~ "Barca",
      # Add more conditions here for other leagues and teams
      TRUE ~ data$Squad
    )

    # Custom theme function
    theme_custom <- function() {
      theme_minimal() +
        theme(plot.background = element_rect(colour = "#0d1117", fill = "#0d1117"),
              panel.background = element_rect(colour = "#0d1117", fill = "#0d1117")) +
        theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
              plot.subtitle = element_markdown(colour = "white", size = 18, hjust = 0.5),
              plot.caption = element_text(colour = "white", size = 12, hjust = 1),
              axis.title.x = element_text(colour = "white", face = "bold", size = 14),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank()) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank()) +
        theme(legend.title = element_text(colour = "black"),
              legend.text = element_text(colour = "black"))
    }

    # Plotting
    g <- ggplot(data = data, aes(axis1 = Squad, axis2 = situation, axis3 = isGoal, y = xG)) +
      scale_x_discrete(limits = c("situation", "grouping", "result")) +
      geom_alluvium(aes(fill = Squad), alpha = 0.7) +
      scale_fill_manual(values = c("#9b3441", "#1f6e9c", "#633372", "#92c051", "#e87b89", "#fe9b00")) +
      geom_stratum(fill = "#0d1117", colour = "white") +
      geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 5, colour = "white") +
      theme_custom() +
      theme(legend.position = "top",
            legend.title = element_text(colour = "white"),
            legend.text = element_text(colour = "white")) +
      labs(title = paste0(input$alluvial_league  ,"Shot (For) Flows"),
           subtitle = paste0(input$alluvial_league  ," | Big Six |", input$alluvial_season,"/", as.numeric(input$alluvial_season)+1,"season"),
           caption = "Data from Understat",
           x = "The size of each block corresponds to the sum of the xG.\nThese Charts attempt to show the shooting trends of teams.")

    return(g)
  })
}

# Wrap the UI and server together
shinyApp(ui = alluvial_ui, server = alluvial_server)
