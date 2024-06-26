# Libraries

library(glue)
library(tidyverse)
library(ggsoccer)
library(TTR)
library(ggtext)
library(patchwork)
library(understatr)
library(hexbin)
library(shiny)
library(shinyWidgets)
library(devtools)
library(ggbraid)

# UI

finishing_ui <- fluidPage(
  tags$head(tags$style(HTML("
  .selectize-input, .selectize-dropdown {
    background-color: black !important;
    color: white !important;
  }
"))),
  
  setBackgroundColor("#14171A"),
  titlePanel(div("Player Finishing Overview", style = "color:#D81B60"), windowTitle = "Player Finishing Overview"),
  # setSliderColor("#D81B60", 1),
  sidebarLayout(
    sidebarPanel(
      textInput("player", "Enter Player Name", value = "Lionel Messi"),
      selectizeInput("situation", "Situation:", choices = c("OpenPlay", "DirectFreekick", "FromCorner", "SetPiece", "Penalty"), multiple = TRUE, selected = c("OpenPlay", "DirectFreekick", "FromCorner", "SetPiece", "Penalty")),
      selectizeInput("shotType", "Shot Type:", choices = c("LeftFoot", "RightFoot", "Head", "OtherBodyPart"), multiple = TRUE, selected = c("LeftFoot", "RightFoot", "Head", "OtherBodyPart")),
      sliderInput("year", "Year:",
                  min = 2014, max = 2021,
                  value = c(2014, 2021),
                  sep = ""),
      numericInput("roll_avg", "Rolling Average of Line Chart:", value = 50),
      selectInput("shots", "Shot Map Type:", choices = c("Point", "Hexbin"), selected = "Point"),
      selectizeInput("plots", "Plots:", choices = c("All", "Line Chart", "Shot Map", "Histogram")),
      radioButtons("theme", "Background Theme:", choices = c("Dark", "Light"), selected = "Dark"),
      downloadButton("download", "Download Plot")
    ),
    mainPanel(plotOutput("plot", height = "800px"))
  )
)

# Server

finishing_server <- function(input, output) {
  
  plot_fun <- reactive({
    
    # Data
    player_names <- read.csv("data/player_name.csv")

    get_player_id <- function(player_name) {
      player_id <- player_names$id[player_names$player_name == player_name]
      return(player_id)
    }

    req(input$player)
    req(input$roll_avg)
    
    dataset <- reactive({
      data <- get_player_shots(get_player_id(input$player))
    }) %>%
      bindCache(input$player)
    
    data <- dataset()
    
    # Theme 
    
    if(input$theme == "Dark") {
      fill_b <- "#212121"
      colorText <- "white"
      colorLine <- "white"
      gridline <- "#525252"
    }
    else if(input$theme == "Light") {
      fill_b <- "floralwhite"
      colorText <- "black"
      colorLine <- "black"
      gridline <- "#9E9E9E"
    }
    
    # Data Wrangling 
    
    data <- data %>%
      mutate(X = X * 120,
             Y = Y * 80) %>%
      mutate(result = ifelse(result == "Goal", "Goal", "No Goal")) %>%
      mutate(isGoal = ifelse(result == "Goal", 1, 0))
    
    line_data <- data %>%
      mutate(GxG = isGoal - xG) %>%
      mutate(GxGSM = TTR::SMA(GxG, n = input$roll_avg)) %>%
      mutate(date = as.Date(date)) %>%
      filter(year >= input$year[1],
             year <= input$year[2],
             situation %in% input$situation,
             shotType %in% input$shotType)
    
    shot_data <- data %>%
      filter(year >= input$year[1],
             year <= input$year[2],
             situation %in% input$situation,
             shotType %in% input$shotType)
    
    hist_data <- data %>%
      filter(year >= input$year[1],
             year <= input$year[2],
             situation %in% input$situation,
             shotType %in% input$shotType)
    
    # Custom Theme
    
    theme_custom <- function() {
      theme_minimal() +
        theme(plot.background = element_rect(colour = fill_b, fill = fill_b),
              panel.background = element_rect(colour = fill_b, fill = fill_b)) +
        theme(plot.title = element_text(colour = colorText, size = 21, face = "bold", hjust = 0.5),
              plot.subtitle = element_markdown(colour = colorText, size = 16, hjust = 0.5),
              plot.caption = element_text(colour = colorText, size = 12, hjust = 1),
              axis.title.x = element_text(colour = colorText, face = "bold", size = 12),
              axis.title.y = element_text(colour = colorText, face = "bold", size = 12),
              axis.text.x = element_text(colour = colorText, size = 8),
              axis.text.y = element_text(colour = colorText, size = 8)) +
        theme(panel.grid.major = element_line(colour = gridline, size = 0.4, linetype = "dashed"),
              panel.grid.minor = element_line(colour = gridline, size = 0.4, linetype = "dashed")) +
        theme(panel.grid.major.x = element_line(colour = gridline, size = 0.4, linetype = "dashed"),
              panel.background = element_blank()) +
        theme(legend.title = element_text(colour = colorText),
              legend.text = element_text(colour = colorText))
    }
    
    # Plot
    
    g1 <- ggplot(line_data, aes(x = date, y = GxGSM)) +
      geom_line(size = 2) +  
      geom_braid(aes(ymin = 0, ymax = GxGSM, fill = GxGSM > 0)) +
      scale_fill_manual(values = c("#D81B60", "#3949AB")) +
      geom_hline(yintercept = 0, size = 1, colour = colorLine, linetype = "longdash") +
      labs(title = glue("{data$player}"), subtitle = glue("{input$year[1]} - {input$year[2]} | League Games Only"), x = glue("{input$roll_avg} Shot Rolling Average"), y = "G - xG") +
      theme_custom() +
      theme(legend.position = "none")
    
    if(input$shots == "Point") {
      g2 <- ggplot() +
        annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = colorLine) +
        coord_flip(xlim = c(60,120),
                   ylim = c(80, -2)) +
        theme_pitch() +
        geom_point(data = shot_data, aes(x = X, y = Y, fill = result, size = xG), colour = colorLine, shape = 21, show.legend = FALSE) +
        scale_fill_manual(values = c("#3949AB", fill_b)) +
        labs(y = glue("{sum(shot_data$isGoal)} Goals with {round(sum(shot_data$xG))} xG\nfrom {nrow(shot_data)} Shots."),
             x = glue("Data via Understat\nAccurate as per {Sys.Date()}")) +
        theme_custom() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y = element_text(size = 8),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
            ) 
    } else if(input$shots == "Hexbin") {
      g2 <- ggplot() +
        annotate_pitch(dimensions = pitch_statsbomb, fill = fill_b, colour = colorLine) +
        coord_flip(xlim = c(60,120),
                   ylim = c(80, -2)) +
        theme_pitch() +
        geom_hex(data = shot_data, aes(x = X, y = Y), bins = 30, colour = colorLine, show.legend = FALSE) +
        scale_fill_gradient(low = "#D81B60", high = "#3949AB") +
        labs(y = glue("{sum(shot_data$isGoal)} Goals with {round(sum(shot_data$xG))} xG\nfrom {nrow(shot_data)} Shots."),
             x = glue("Data via Understat\nAccurate as per {Sys.Date()}")) +
        theme_custom() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.title.y = element_text(size = 8),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              )
    }
    
    g3 <- ggplot() +
      geom_histogram(data = hist_data, aes(x = xG, fill = result), bins = 20, position = position_stack(reverse = TRUE)) +
      scale_fill_manual(values = c("#3949AB", "#D81B60")) +
      labs(x = "Individual Shot xG",
           y = "Frequency") +
      geom_vline(xintercept = mean(hist_data$xG), colour = colorLine, size = 0.5, linetype = "longdash") +
      annotate(geom = "text", x = mean(hist_data$xG) + 0.02, y = 50, label = glue("xG/Shot=", round(mean(hist_data$xG), 2)), colour = colorText, size = 4) +
      theme_custom() +
      theme(legend.position = c(0.9, 0.9),
            legend.title = element_blank())
    
    if ("All" %in% input$plots) {
      
      my_plot <- g1 / g2 / g3
      my_plot &
        plot_annotation(,
                        theme = theme(plot.background = element_rect(fill = fill_b, colour = fill_b),
                                      plot.caption = element_text(colour = colorText, hjust = 1, size = 10)))
      
    } else if ("Line Chart" %in% input$plots) {
      
      g1 &
        plot_annotation(
                        theme = theme(plot.background = element_rect(fill = fill_b, colour = fill_b),
                                      plot.caption = element_text(colour = colorText, hjust = 1, size = 10)))
      
    } else if ("Shot Map" %in% input$plots) {
      
      g2 +
        theme(axis.title.y = element_blank()) &
        plot_annotation(
                        title = glue("{data$player}"), subtitle = glue("{input$year[1]} - {input$year[2]} | League Games Only"),
                        theme = theme(plot.background = element_rect(fill = fill_b, colour = fill_b),
                                      plot.caption = element_text(colour = colorText, hjust = 1, size = 10),
                                      plot.title = element_text(colour = colorText, hjust = 0.5, size = 21, face = "bold"),
                                      plot.subtitle = element_text(colour = colorText, hjust = 0.5, size = 16)))
      
    } else if ("Histogram" %in% input$plots) {
      
      g3 &
        plot_annotation(
                        title = glue("{data$player}"), subtitle = glue("{input$year[1]} - {input$year[2]} | League Games Only"),
                        theme = theme(plot.background = element_rect(fill = fill_b, colour = fill_b),
                                      plot.caption = element_text(colour = colorText, hjust = 1, size = 10),
                                      plot.title = element_text(colour = colorText, hjust = 0.5, size = 21, face = "bold"),
                                      plot.subtitle = element_text(colour = colorText, hjust = 0.5, size = 16)))
      
    }
  })
  
  output$plot <- renderPlot({
    
    plot_fun()
    
  })
  
  # Download
  
  output$download <- downloadHandler(
    filename = function() { paste(input$player, ".png", sep="") },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 10, height = height, res = 300, units = "in")
      ggsave(file, plot = plot_fun(), device = device, bg = "#212121")
    }
  )
}

shinyApp(ui = finishing_ui, server = finishing_server)