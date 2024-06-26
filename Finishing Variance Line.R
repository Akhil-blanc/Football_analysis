# Loading necessary packages 

library(dplyr)
library(RcppRoll)
library(ggplot2)
library(ggtext)
library(devtools)

# Install understatr from GitHub
install_github("ewenme/understatr")
library(understatr)

# Data extraction & manipulation



data <- get_player_shots(755)
data <- data %>%
    filter(!situation == "Penalty")

data$result[data$result == "Goal"] <- 1
data$result[data$result == "MissedShots"] <- 0
data$result[data$result == "SavedShot"] <- 0
data$result[data$result == "BlockedShot"] <- 0
data$result[data$result == "ShotOnPost"] <- 0
data$result[data$result == "OwnGoal"] <- 0

data <- data %>%
    mutate(goals = as.numeric(result),
                 GxG = goals - xG,
                 GxGSM = TTR::SMA(GxG, n = 50),
                 no = 1:nrow(data))

# Plotting function

generate_plot <- function() {
    attach(data)

    plot <- ggplot(data, aes(x = no, y = GxGSM, colour = GxGSM)) +
        geom_line(size = 3) + geom_point(size = 3) + geom_smooth(colour = "white", size = 2) +
        scale_colour_gradient2(low = "red", mid = "yellow" , high = "seagreen") +
        theme(legend.position="none") +
        labs(x=NULL, y=NULL, 
                 title="<b style='color:#ffffff'>Jamie Vardy Finishing Variance</b>",
                 subtitle="<b style='color:#ffffff'>50 Shot Rolling Average (Non-Penalty shots)</b>") +
        theme_classic(base_size = 24) +
        theme(plot.title = element_markdown(lineheight = 1.1),
                    plot.subtitle = element_markdown(lineheight = 1.1)) +
        labs(fill="",                                                                       
                 caption = "Data from Understat") +
        theme(plot.background = element_rect(fill = "#000000", colour = "#000000")) +
        theme(panel.background = element_rect(fill = "#000000", colour = "#000000")) +
        theme(plot.caption = element_text(color = "white")) +
        labs(x = "Shots Taken", y = "NPGoals - NPxG") +
        theme(axis.title.x = element_text(colour = "#ffffff")) +
        theme(axis.title.y = element_text(colour = "#ffffff")) +
        theme(axis.text.x = element_text(colour = "#ffffff"),
                    axis.text.y = element_text(colour = "#ffffff")) +
        theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()) +
        theme(panel.grid.major.y = element_blank()) + 
        theme(axis.line = element_line(size = 0.8, colour = "#ffffff")) +
        theme(legend.position = "none") +
        geom_hline(yintercept = 0, linetype = "dashed", size = 1, colour = "#ffffff") +
        xlim(50, 523)

    return(plot)
}

# Save the plot
ggsave("Finishing Variance Line.png", plot = generate_plot(), width = 12, height = 8, dpi = 300)