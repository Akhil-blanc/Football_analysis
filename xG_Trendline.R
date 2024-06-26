# Loading necessary packages

library(dplyr)
library(RcppRoll)
library(ggplot2)
library(ggpubr)
library(ggtext)
library(worldfootballR)

#' Data extraction and manipulation. This part will cover data 
#' from two sources, Understat & FBref. Understat data is
#' generally considered to be less robust than FBref data,
#' but dates back to 2014, giving a larger timeframe than FBref via StatsBomb

# Understat
print("scrapping data")
data1 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2014")
print("data1 scrapped")
data2 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2015")
print("data2 scrapped")
data3 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2016")
print("data3 scrapped")
data4 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2017")
print("data4 scrapped")
data5 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2018")
print("data5 scrapped")
data6 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2019")
print("data6 scrapped")
data7 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2020")
print("data7 scrapped")
data8 <- understat_team_season_shots(team_url = "https://understat.com/team/Manchester_United/2021")
print("data scrapped")

data <- rbind(data1, data2, data3, data4, data5, data6, data7, data8)

df1 <- data %>%
  filter(home_away == "h") %>%
  filter(home_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
df2 <- data %>%
  filter(home_away == "a") %>%
  filter(away_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
df <- rbind(df1, df2)

df3 <- data %>%
  filter(home_away == "a") %>%
  filter(home_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
df4 <- data %>%
  filter(home_away == "h") %>%
  filter(away_team == "Manchester United") %>%
  group_by(date) %>%
  summarise(sum(xG))
dfa <- rbind(df3, df4)
dfa <- dfa[, 2]
dfa <- dfa %>%
  rename(xGA = 'sum(xG)')

data <- cbind(df, dfa)
data <- data %>%
  rename(xG = 'sum(xG)') %>%
  mutate(xGSUM = (xG + xGA)/2)
data <- data[order(as.Date(data$date),decreasing = FALSE),]
data <- data %>%
  mutate(Round = 1:nrow(data),
         xGSM = TTR::SMA(xG, n = 15),
         xGASM = TTR::SMA(xGA, n = 15),
         xGSUM = TTR::SMA(xGSUM, n = 15))

# FBref (via StatsBomb)

# dat1 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2018, tier = "1st")
# dat2 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2019, tier = "1st")
# dat3 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2020, tier = "1st")
# dat4 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2021, tier = "1st")
# dat5 <- get_match_results(country = "ENG", gender = "M", season_end_year = 2022, tier = "1st")

# dat5 <- dat5[c(1:65), ]
# data <- rbind(dat1, dat2, dat3, dat4, dat5)

# df1 <- data %>%
#   filter(Home == "Manchester Utd")
# df1 <- df1[, c("Date","Home", "Home_xG")]
# df1 <- df1 %>%
#   rename(xG = Home_xG) %>%
#   rename(Team = Home)
# df2 <- data %>%
#   filter(Away == "Manchester Utd")
# df2 <- df2[, c("Date","Away", "Away_xG")]
# df2 <- df2 %>%
#   rename(xG = Away_xG) %>%
#   rename(Team = Away)
# df <- rbind(df1, df2)

# df3 <- data %>%
#   filter(Home == "Manchester Utd")
# df3 <- df3[, c("Date","Away", "Away_xG")]
# df3 <- df3 %>%
#   rename(xGA = Away_xG) %>%
#   rename(Team = Away)
# df4 <- data %>%
#   filter(Away == "Manchester Utd")
# df4 <- df4[, c("Date","Home", "Home_xG")]
# df4 <- df4 %>%
#   rename(xGA = Home_xG) %>%
#   rename(Team = Home)
# dfa <- rbind(df3, df4)
# dfa <- dfa[, 3]
# data <- cbind(df, dfa)
# data <- data %>%
#   rename(xGA = dfa) %>%
#   mutate(xGSUM = (xG + xGA)/2)

# data <- data[order(as.Date(data$Date),decreasing = FALSE),]
# data <- data %>%
#   mutate(Round = 1:nrow(data),
#          xGSM = TTR::SMA(xG, n = 10),
#          xGASM = TTR::SMA(xGA, n = 10),
#          xGSUM = TTR::SMA(xGSUM, n = 10))

# Plotting

attach(data)

fig <- ggplot(data , aes(x = Round)) +
  geom_line(aes(y = xGSM), colour = "#238443", size = 3) +
  geom_line(aes(y = xGASM), colour = "#cb181d", size = 3) +
  geom_line(aes(y = xGSUM), colour = "#000000", size = 0.1) +  
  geom_point(aes(y = xGSM), colour = "#238443", size = 4) +
  geom_point(aes(y = xGASM), colour = "#cb181d", size = 4) +
  expand_limits(y = c(0.25, 2.25)) +
  stat_smooth(method = 'lm', aes(y = xGSM), color = "#238443", linetype ="dashed",alpha = 0.5, size = 2,se = FALSE)+
  stat_smooth(method = 'lm', aes(y = xGA), color = "#cb181d", linetype= "dashed", alpha = 0.5, size = 2,se = FALSE) +
  labs(x=NULL, y=NULL, 
       title="<b style='color:#ffffff'>Manchester United xG Trendline (2014-) </b>", 
       subtitle="<b style='color:#ffffff'>15 game rolling average.</b> <b style='color:#238443'>xG</b> <b style='color:#ffffff'>|</b> 
<b style='color:#cb181d'>xGA</b>") +
  theme_classic(base_size = 24) +
  theme(plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1)) +
  labs(fill="",                                                                       
       caption = "Data from Understat") +
  theme(plot.background = element_rect(fill = "#000000", colour = "#000000")) +
  theme(panel.background = element_rect(fill = "#000000", colour = "#000000")) +
  theme(plot.caption = element_text(color = "white")) +
  labs(x = "Matchweek", y = "xG") +
  theme(axis.title.x = element_text(colour = "#ffffff")) +
  theme(axis.title.y = element_text(colour = "#ffffff")) +
  theme(axis.text.x = element_text(colour = "#ffffff"),
        axis.text.y = element_text(colour = "#ffffff")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = c(0, 50, 100, 150, 200, 250, 300)) +
  theme(panel.grid.major.y = element_blank()) + 
  theme(axis.line = element_line(size = 0.8, colour = "#ffffff")) +
  geom_ribbon(aes(ymin=xGASM, ymax=xGSUM, x=Round), fill = "#cb181d", alpha = 0.4) +
  geom_ribbon(aes(ymin=xGSUM, ymax=xGSM, x=Round), fill = "#238443", alpha = 0.4) +
  stat_smooth(method = 'lm', aes(y = xGSM), color = "#238443", linetype ="dashed",alpha = 0.5, size = 2,se = FALSE)+
  stat_smooth(method = 'lm', aes(y = xGA), color = "#cb181d", linetype= "dashed", alpha = 0.5, size = 2,se = FALSE) +
  theme(legend.position = "none") +
  geom_vline(xintercept = 36, linetype = "solid", colour = "#ffffff", size = 2) +
  geom_vline(xintercept = 74, linetype = "solid", colour = "#ffffff", size = 2) +
  geom_vline(xintercept = 112, linetype = "solid", colour = "#ffffff", size = 2) +
  geom_vline(xintercept = 149, linetype = "solid", colour = "#ffffff", size = 2) +
  geom_vline(xintercept = 189, linetype = "solid", colour = "#ffffff", size = 2) +
  geom_vline(xintercept = 225, linetype = "solid", colour = "#ffffff", size = 2) +
  geom_vline(xintercept = 263, linetype = "solid", colour = "#ffffff", size = 2)

ggsave("xG_Trendline.png", plot = fig, width = 20, height = 10, units = "in", dpi = 300)