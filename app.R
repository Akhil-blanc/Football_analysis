library(shiny)
library(tidyverse)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggalt)

# Source app1.R
source("Waffle.R")

# Source app2.R
source("Dumbell_Chart.R")

source("StackedBar.R")

source("Alluvial.R")

source("Basic Scatterplot.R")

source("Player_Finishing.R")

# Define combined UI
ui <- fluidPage(
  titlePanel("FootyViz - One Stop Shop for Football Visualizations"),
  tabsetPanel(
    tabPanel("Player Stats", finishing_ui),
    tabPanel("Dumbell Chart", dumbell_ui),
    tabPanel("Stacked Bar Chart", stackedbar_ui),
    tabPanel("Alluvial Chart", alluvial_ui),
    tabPanel("Waffle Chart", waffle_ui),
    tabPanel("Basic Scatterplot", scatter_ui),
  )
)

# Define combined server
server <- function(input, output) {
  waffle_server(input, output)
  dumbell_server(input, output)
  stackedbar_server(input, output)
  alluvial_server(input, output)
  scatter_server(input, output)
  finishing_server(input, output)

}

# Run the combined app
shinyApp(ui = ui, server = server)