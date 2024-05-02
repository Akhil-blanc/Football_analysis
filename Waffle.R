# Libraries
library(shiny)
library(tidyverse)
library(worldfootballR)
library(ggtext)
library(waffle)
library(MetBrewer)
library(extrafont)

theme_athletic <- function() {
  theme_minimal() +
    theme(plot.background = element_rect(colour = "#151515", fill = "#151515"),
          panel.background = element_rect(colour = "#151515", fill = "#151515")) +
    theme(plot.title = element_text(colour = "white", size = 18, family = "Fried Chicken Bold", hjust = 0.5),
          plot.subtitle = element_markdown(colour = "#525252", size = 18, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 15, hjust = 1),
          axis.title.x = element_text(colour = "white", face = "bold", size = 14),
          axis.title.y = element_text(colour = "white", face = "bold", size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    theme(legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"))
}

# Define UI
waffle_ui <- fluidPage(
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
  
      # titlePanel("Shot Creating Actions"),
  
  # Layout with columns
  fluidRow(
    column(width = 3,  # Adjust this value to change the sidebar width
      wellPanel(
      selectInput("season", "Select Season Year:",
                  choices = c("2019", "2020", "2021", "2022", "2023"),
                  selected = "2019"),
      sliderInput("threshold", "Minimum 90's Played:",
                  min = 7, max = 38, value = 20),
      sliderInput("top_players", "Top Players:",
                  min = 5, max = 20, value = 10)
      )
    ),
    column(width = 9,  # Adjust this value to change the main panel width
      plotOutput("wafflePlot", height = "790px")
    )
  )
)

# Define server logic
waffle_server <- function(input, output) {
  
  data <- reactive({
    file_path <- paste0("scatter_data/scatter_data_", input$season, ".csv")
    data <- read.csv(file_path)
    return(data)
  })
  
  output$wafflePlot <- renderPlot({
    data1 <- data() %>%
      filter(Mins_Per_90 >= input$threshold) %>%
      select(Player, Mins_Per_90, SCA90_SCA, SCA_SCA, PassLive_SCA, PassDead_SCA, Sh_SCA, Fld_SCA, Def_SCA)
      data1 <- data1[order(as.numeric(data1$SCA90_SCA),decreasing = TRUE),]
      data1 <- data1[c(1:input$top_players),]

      df <- data1[order(as.numeric(data1$SCA90_SCA),decreasing = TRUE),]
      df <- df[c(1:input$top_players),]

      Player <- data1$Player
      Mins <- data1$Mins_Per_90
      data1 <- subset(data1, select = -c(Player, Mins_Per_90, SCA90_SCA))

      for(i in 1:ncol(data1)) {
      data1[, i] <- data1[, i] / Mins
      }

      SCA <- data1$SCA_SCA 

      for(i in 1:ncol(data1)) {
      data1[, i] <- round((data1[, i] / SCA) * 100, 0)
      }

      data1 <- data1 %>%
      mutate(Total = PassLive_SCA + PassDead_SCA  + Sh_SCA + Fld_SCA + Def_SCA)

      ## run this ifelse satatement as many times as necessarry until the Total comes out to be a 100 for all rows.

      data1 <- data1 %>% mutate(Sh_SCA = ifelse(Total == 100, Sh_SCA,
                                                ifelse(Total < 100, Sh_SCA + 1,
                                                      ifelse(Total > 100, Sh_SCA - 1, NA)))) %>% 
      mutate(Total = PassLive_SCA + PassDead_SCA + Sh_SCA + Fld_SCA + Def_SCA)

      data1$Player <- Player 

      data1 <- data1 %>%
      pivot_longer(!Player, values_to = "SCAp90", names_to = "SCATypes") %>%
      filter(!SCATypes == "SCA_SCA") %>%
      filter(!SCATypes == "Total") %>%
      count(Player, SCATypes, wt = SCAp90)

      data1$Player <- factor(data1$Player, levels = print(df$Player))
    
    # Rest of your data wrangling and plotting code...
      p <-data1 %>%
      ggplot(aes(fill = SCATypes, values = n)) +
      geom_waffle(nrows = 10, size = 1.5, colour = "#151515", flip = TRUE) +
      scale_fill_manual(values = met.brewer(name = "Gauguin", n = 6, type = "discrete")) +
      facet_wrap(~Player) +
      labs(title = paste("Big 5 Leagues Shot-Creating Actions Share [", input$season, "/", as.numeric(input$season)+1 ,"]", sep = ""),
      subtitle = paste("Top", input$top_players, "Players with the most SCA per 90 so far"),
      caption = "Minimum 9 90's Played\nData from FBref") +
      theme_athletic() +
      theme(aspect.ratio = 1,
            strip.background = element_blank(),
            strip.text = element_text(colour = "white", size = 14),
            legend.position = "top",
            legend.text = element_text(size = 14))
    return(p)
  })
}

# Run the application
shinyApp(ui = waffle_ui, server = waffle_server)