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

# Define combined UI
ui <- fluidPage(
  titlePanel("Combined App"),
  tabsetPanel(
    tabPanel("Waffle Chart", waffle_ui),
    tabPanel("Dumbell Chart", dumbell_ui)
  )
)

# Define combined server
server <- function(input, output, session) {
  waffle_server(input, output, session)
  dumbell_server(input, output, session)
}

# Run the combined app
shinyApp(ui = ui, server = server)