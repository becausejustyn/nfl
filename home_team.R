

pbp_2021 <- read_rds("pbp_2021.rds")

pbp <- pbp_2021 %>%
  select(game_id, season, home_team, home_score, away_team, away_score, week, game_date)

pbp <- pbp %>%
  group_by(game_id) %>%
  #keeps only the last row
  slice(n()) %>%
  summarise(
    winner = if_else(home_score > away_score, home_team, away_team),
    loser = if_else(away_score < home_score, away_team, home_team),
    date = as.Date(game_date),
    play_week = week, 
    points_winner = if_else(home_score > away_score, home_score, away_score),
    points_loser = if_else(away_score < home_score, away_score, home_score),
    score_delta = points_winner - points_loser,
    # did the home team win
    is_home = if_else(home_score > away_score, 1, -1),
    #adjust this with more data
    period = season,
    .groups = "drop"
  ) %>%
  select(-game_id)

pbp %>%
  ggplot(aes(
    x = score_delta
    )) +
  geom_histogram(bins = 30L) +
  scale_x_continuous(breaks = seq(0, 35, 7))
  labs(
    x = "Score Difference",
    y = "Frequency"
  )
