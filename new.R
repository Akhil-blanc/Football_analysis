library(worldfootballR)

leagues <- c("EPL", "La Liga", "Serie A", "Bundesliga", "Ligue 1")
seasons <- c(2022, 2023)

for (league in leagues) {
  for (season in seasons) {
    print(paste("Downloading data for", league, "season", season))
    data <- understat_league_season_shots(league = league, season_start_year = season)
    filename <- paste0(league, "_", season, "_shots.csv")
    write.csv(data, filename)
    print(paste("Data saved to", filename))
  }
}
