# Load required libraries
library(shiny)

# Get a list of all R scripts in the current directory
plot_files <- list.files(pattern = "Dumbell_Chart.R")

# Create a UI for the application
ui <- fluidPage(
    # Input for parameters
    textInput("param", "Enter parameter:"),
    
    # Create a tabset
    do.call(tabsetPanel,
        # Use lapply to create a tabPanel for each plot
        lapply(plot_files, function(file) {
            tabPanel(file,
                plotOutput(file)
            )
        })
    )
)

# Define server logic
server <- function(input, output) {
    # Use lapply to create a renderPlot for each plot
    lapply(plot_files, function(file) {
        output[[file]] <- renderPlot({
            # Source the R script for the plot, passing in the parameter
            source(file, local = TRUE)
            generate_plot()
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)