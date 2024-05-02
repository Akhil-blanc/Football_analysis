# Load necessary libraries
library(shiny)
library(worldfootballR)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(extrafont)

# Define UI
ui <- fluidPage(
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
  ")),
  
      # titlePanel("Shot Creating Actions"),
  
  # Layout with columns
  fluidRow(
    column(width = 3,  # Adjust this value to change the sidebar width
      wellPanel(
        selectInput("season", "Select Season Year:",
                    choices = c("2019", "2020", "2021", "2022", "2023"),
                    selected = "2019"),
      selectInput("position", "Select Position:",
            choices = c("FW", "MF", "DF", "FW,MF"),
            selected = "FW"),
      sliderInput("threshold", "Minimum 90's Played:",
                  min = 7, max = 38, value = 20)
      )
    ),
    column(width = 9,  # Adjust this value to change the main panel width
      plotOutput("scatterplot", height = "900px", width = "1000px")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  data <- reactive({
      file_path <- paste0("scatter_data/scatter_data_", input$season, ".csv")
      data <- read.csv(file_path)
      return(data)
      })

 
  # Create scatterplot
  output$scatterplot <- renderPlot({
    data_filtered <- data() %>%
      filter(Mins_Per_90 >= input$threshold) %>%
      filter(Pos == input$position) %>%
      mutate(Dribble = Sh_SCA/Mins_Per_90) %>%
      mutate(Total = SCA_SCA/Mins_Per_90)
    
    ggplot(data_filtered) +
      geom_point(aes(x = Total, y = Dribble), colour = "#4292c6", size = 3) +
      geom_point(data = data_filtered[111, ], aes(x = Total, y = Dribble), colour = "#fc9272", size = 9) +
      geom_text_repel(aes(x = Total, y = Dribble, label = Player),
                      box.padding   = 0.35, 
                      point.padding = 1.5,
                      segment.color = "black",
                      colour = "white", 
                      alpha = 1) +
      labs(title = "Shot Creating Actions",
           subtitle = paste("Forwards | Big 5 Leagues", input$season, "/", as.numeric(input$season) + 1, " | Minimum 20 90's played"),
           caption = "Data from FBref") +
      theme_athletic()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
