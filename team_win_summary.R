

pbp <- purrr::map_df(c(2001:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )})


# Create outcomes dataframe
outcomes <- pbp %>%
  filter(season_type == 'REG') %>% 
  group_by(season, game_id, home_team) %>%
  summarise(
    home_win = if_else(sum(result) > 0, 1, 0),
    home_tie = if_else(sum(result) == 0, 1, 0),
    home_diff = last(result),
    home_pts_for = last(home_score),
    home_pts_against = last(away_score)
  ) %>%
  group_by(season, home_team) %>%
  summarise(
    home_games = n(),
    home_wins = sum(home_win),
    home_ties = sum(home_tie),
    home_diff = sum(home_diff),
    home_pts_for = sum(home_pts_for),
    home_pts_against = sum(home_pts_against),
    .groups = "drop"
  ) %>%
  left_join(
    # away games
      filter(pbp, season_type == 'REG') %>%
      group_by(season, game_id, away_team) %>%
      summarise(
        away_win = if_else(sum(result) < 0, 1, 0),
        away_tie = if_else(sum(result) == 0, 1, 0),
        away_diff = last(result)*-1,
        away_pts_for = last(away_score),
        away_pts_against = last(home_score)
      ) %>%
      group_by(season, away_team) %>%
      summarise(
        away_games = n(),
        away_wins = sum(away_win),
        away_ties = sum(away_tie),
        away_diff = sum(away_diff),
        away_pts_for = sum(away_pts_for),
        away_pts_against = sum(away_pts_against),
        .groups = "drop"
      ),
    by = c("season", "home_team" = "away_team")
  ) %>%
  rename(team = "home_team") %>%
  mutate(
    games = home_games + away_games,
    wins = home_wins + away_wins,
    losses = games - wins,
    ties = home_ties + away_ties,
    win_percentage = (wins + 0.5 * ties) / games,
    point_diff = home_diff + away_diff,
    points_for = home_pts_for + away_pts_for,
    points_against = home_pts_against + away_pts_against,
    pythag_wins = (points_for^2.37 / (points_for^2.37 + points_against^2.37))*games
  ) %>%
  select(
    season, team, games, wins, losses, ties, win_percentage, point_diff, points_for, points_against, pythag_wins
  )

