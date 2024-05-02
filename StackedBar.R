# Libraries
library(shiny)
library(tidyverse)
library(worldfootballR)
library(extrafont)
library(ggtext)
library(MetBrewer)
library(ggpubr)
library(future)
library(purrr)
library(plotly)
library(ggplot2)
library(viridis)

# Define UI
stackedbar_ui <- fluidPage(
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

      titlePanel("xG Performance by Time Period"),
  
  fluidRow(
    column(width = 3, 
      wellPanel(
        selectInput("stack_season", "Select Season Year:",
                    choices = c("2020", "2021"),
                    selected = "2020"),
        selectInput("stack_league", "Select League:",
                    choices = c("EPL", "La liga", "Serie A", "Bundesliga", "Ligue 1"),
                    selected = "Ligue 1"),
      )
    ),
    column(width = 9, 
      plotOutput("stackedbarPlot", height = "790px", width = "100%")
    )
  )
)

# Define server logic
stackedbar_server <- function(input, output) {
  
  data <- reactive({
    file_path <- paste0("data/season_shots/", input$stack_league, "_", input$stack_season, "_shots.csv")
    data <- read.csv(file_path)
    return(data)
  })

  output$stackedbarPlot <- renderPlot({
    req(data()) 
    
    data <- data()
    data$minute <- as.numeric(data$minute)

    data <- data %>%
      mutate(Period = ifelse(minute <= 15, "0-15",
                             ifelse(minute > 15 & minute <= 30, "15-30",
                                    ifelse(minute > 30 & minute <= 45, "30-45", 
                                           ifelse(minute > 45 & minute <= 60, "45-60",
                                                  ifelse(minute > 60 & minute <= 75, "60-75",        
                                                         ifelse(minute > 75 & minute < 100, "75-90", NA)))))))
    
    data1 <- data %>%
      filter(h_a == "h") %>%
      group_by(home_team, Period) %>%
      summarise(xG = sum(xG)) %>%
      spread(Period, xG) %>%
      ungroup() %>%
      rename(Squad = home_team)
    
    data2 <- data %>%
      filter(h_a == "a") %>%
      group_by(away_team, Period) %>%
      summarise(xG = sum(xG)) %>%
      spread(Period, xG) %>%
      ungroup() %>%
      rename(Squad = away_team)
    
    data3 <- data %>%
      filter(h_a == "h") %>%
      group_by(away_team, Period) %>%
      summarise(xG = sum(xG)) %>%
      spread(Period, xG) %>%
      ungroup() %>%
      rename(Squad = away_team)
    
    data4 <- data %>%
      filter(h_a == "a") %>%
      group_by(home_team, Period) %>%
      summarise(xG = sum(xG)) %>%
      spread(Period, xG) %>%
      ungroup() %>%
      rename(Squad = home_team)
    
    Squad <- data1$Squad
    
    data1 <- subset(data1, select = -Squad)
    data2 <- subset(data2, select = -Squad)
    data3 <- subset(data3, select = -Squad)
    data4 <- subset(data4, select = -Squad)
    
    for(i in 1:ncol(data1)) {
      data1[, i] <- data1[, i] + data2[, i]
    }
    
    for(i in 1:ncol(data3)) {
      data3[, i] <- data3[, i] + data4[, i]
    }
    
    data1$Squad <- Squad
    data3$Squad <- Squad
    
    data1 <- data1 %>% pivot_longer(!Squad, names_to = "Period", values_to = "xG")
    data3 <- data3 %>% pivot_longer(!Squad, names_to = "Period", values_to = "xG")
    
    df1 <- data1 %>%
      filter(case_when(
        input$stack_league == "Ligue 1" ~ Squad == "Angers",
        input$stack_league == "EPL" ~ Squad == "Arsenal",
        input$stack_league == "La liga" ~ Squad == "Barcelona",
        input$stack_league == "Serie A" ~ Squad == "Atalanta",
        input$stack_league == "Bundesliga" ~ Squad == "Bayern Munich",
        TRUE ~ TRUE
      ))

df1 <- df1[order(df1$Period, decreasing = TRUE),]
data1$Period <- factor(data1$Period, levels= unique(df1$Period))

df3 <- data3 %>%
      filter(case_when(
        input$stack_league == "Ligue 1" ~ Squad == "Angers",
        input$stack_league == "EPL" ~ Squad == "Arsenal",
        input$stack_league == "La liga" ~ Squad == "Barcelona",
        input$stack_league == "Serie A" ~ Squad == "Atalanta",
        input$stack_league == "Bundesliga" ~ Squad == "Bayern Munich",
        TRUE ~ TRUE
      ))

df3 <- df3[order(df3$Period, decreasing = TRUE),]
data3$Period <- factor(data3$Period, levels= unique(df3$Period))
    
    
    theme_athletic <- function() {
      theme_minimal() +
        theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
              panel.background = element_rect(colour = "#151515", fill = "#151515")) +
        theme(plot.title = element_text(colour = "white", size = 24, family = "Fried Chicken Bold", hjust = 0.5),
              plot.subtitle = element_markdown(colour = "#525252", size = 18, hjust = 0.5),
              plot.caption = element_text(colour = "white", size = 12, hjust = 1),
              axis.title.x = element_text(colour = "white", face = "bold", size = 14),
              axis.title.y = element_text(colour = "white", face = "bold", size = 14),
              axis.text.x = element_text(colour = "white", size = 12),
              axis.text.y = element_text(colour = "white", size = 12)) +
        theme(panel.grid.major = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
              panel.grid.minor = element_line(colour = "#525252", size = 0.4, linetype = "dashed")) +
        theme(panel.grid.major.x = element_line(colour = "#525252", size = 0.4, linetype = "dashed"),
              panel.background = element_blank()) +
        theme(legend.title = element_text(colour = "white"),
              legend.text = element_text(colour = "white"))
    }
    
    # Plotting
    
    # Plotting
p1 <- ggplot(data1, aes(x = reorder(Squad, xG), y = xG, fill = Period)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d() + # Use viridis discrete color palette
  labs(title = "xG For", x = "Squad", y = "xG") +
  coord_flip() +
  theme_athletic()

p2 <- ggplot(data3, aes(x = reorder(Squad, -xG), y = xG, fill = Period)) +
  geom_bar(stat="identity") +
  scale_fill_viridis_d() + # Use viridis discrete color palette
  labs(title = "xG Against", x = "Squad", y = "xGA") +
  coord_flip() +
  theme_athletic() +
  theme(axis.title.y = element_blank())
    
    # Combining
    
    fig <- ggarrange(p1, p2, ncol = 2, nrow = 1)
    fig +
      theme_athletic() +
      labs(title = paste0( input$stack_league,"xG Performance by Time Period"),
           subtitle = paste0(input$stack_season,"/", as.numeric(input$stack_season)+1,"season"), 
           caption = "Data from Understat")
    
    # Save
    return(fig)
    # setwd("C:/Users/91934/Documents/GitHub/R-Code")
    ggsave("bar1.png", width = 4000, height = 2000, units = "px")
  })
}

# Run the application 
shinyApp(ui = stackedbar_ui, server = stackedbar_server)
