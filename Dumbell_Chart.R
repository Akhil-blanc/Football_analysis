
# Load necessary libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(ggalt)


# Define UI
dumbell_ui <- fluidPage(
  tags$style(HTML("
    .shiny-output-error { visibility: hidden; }
    .shiny-output-error:before { visibility: hidden; }
    body {
      background-color: black;
      color: white;
    }
    .well {
      background-color: black;
    }
  ")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("country", "Select Country:",
                  choices = c("FRA", "ENG", "ESP", "ITA", "GER"), 
                  selected = "FRA"),
      selectInput("gender", "Select Gender:",
                  choices = c("M", "F"), 
                  selected = "M"),
      selectInput("season_end_year", "Select Season End Year:",
                  choices = c("2019", "2020", "2021", "2022", "2023"), 
                  selected = "2021"),
      selectInput("tier", "Select Tier:",
                  choices = c("1st", "2nd"), 
                  selected = "1st")
    ),
    mainPanel(
      width = 9,
      plotOutput("dumbbellPlot", height = "900px", width = "1000px")
    )
  )
)

# Define server logic
dumbell_server <- function(input, output, session) {
  output$dumbbellPlot <- renderPlot({
    # Data retrieval
    filename <- paste0("league_data/",input$country, "_", input$gender, "_", input$season_end_year, "_", input$tier, "_league_table.csv")
    league_table <- read.csv(filename)

    competition_name <- league_table[,c(1)]
    data <- league_table[, c(5, 14, 19, 27, 32)]
    data$GD_Home <- as.numeric(data$GD_Home)
    data$xGD_Home <- as.numeric(data$xGD_Home)
    data$GD_Away <- as.numeric(data$GD_Away)
    data$xGD_Away <- as.numeric(data$xGD_Away)
    data <- data %>%
      mutate(xGDiff_Home = GD_Home - xGD_Home,
             xGDiff_Away = GD_Away - xGD_Away)
    data1 <- data[, c(1, 6, 7)]
    data1$Squad <- factor(data1$Squad, levels=as.character(data1$Squad))
    
    # Plotting
    DBplot <- ggplot(data1, aes(x=xGDiff_Home, xend=xGDiff_Away, y=Squad)) +
      geom_segment(aes(x=xGDiff_Home, 
                       xend=xGDiff_Away, 
                       y=Squad, 
                       yend=Squad), 
                   color="#b2b2b2", size=3) +
      geom_dumbbell(color="light blue",         
                    size_x=10,                  
                    size_xend =10,            
                    colour_x="#238b45",       
                    colour_xend = "#fe9929") +
      labs(x=NULL, y=NULL, 
       title=paste("<b style='color:#ffffff'>Overperformance or Underperformance ? (", input$season_end_year, "/", as.numeric(input$season_end_year)+1 ,")</b>", sep=""), 
       subtitle=paste("<b style='color:#ffffff'>How have", competition_name, "teams performed on their Goal Difference</b> 
       <b style='color:#238b45'>Home</b> <b style='color:#ffffff'>&</b> 
       <b style='color:#fe9929'>Away</b> <b style='color:#ffffff'>?</b>", sep=" "),
       fill="",                                                                       
       caption = "Data from FBref via StatsBomb, WorldFootballR") +
 
      theme_classic(base_size = 24) +
      theme(plot.title = element_markdown(lineheight = 1.1),
            plot.subtitle = element_markdown(lineheight = 1.1),
            plot.background = element_rect(fill = "#000000", colour = "#000000"),
            panel.background = element_rect(fill = "#000000", colour = "#000000"),
            plot.caption = element_text(color = "white"),
            axis.title.x = element_text(colour = "#ffffff"),
            axis.text.x = element_text(colour = "#ffffff"),
            axis.text.y = element_text(colour = "#ffffff"),
            panel.grid.major = element_line(colour="grey", size = (0.1)),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank(),
            axis.line = element_line(size = 0.8, colour = "#ffffff"))
    DBplot
  })
}

# Run the application
shinyApp(ui = dumbell_ui, server = dumbell_server)
