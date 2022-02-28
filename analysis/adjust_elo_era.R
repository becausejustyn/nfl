
library(tidyverse)

nfl_elo_latest <- read_csv("Downloads/nfl-elo/nfl_elo_latest.csv")

nfl_elo <- nfl_elo_latest %>%
  filter(
    !is.na(qbelo1_pre),
    !is.na(qbelo2_pre),
    is.na(playoff) #exclude playoffs
    )

pbp <- read_rds("~/pbp_2021.rds")

pbp <- pbp %>% 
  select(game_date, week) %>% 
  distinct(game_date, .keep_all = TRUE) %>% 
  mutate(game_date = lubridate::as_date(game_date))

nfl_elo <- nfl_elo %>%
  left_join(pbp, by = c("date" = "game_date")) %>%
  relocate(week, .after = date)

# home

nfl_elo_home <- nfl_elo %>%
  select(
    season, week, 
    qb_name = qb1, qb_elo_value = qb1_value_post, 
    points_for = score1, points_against = score2
  )

#away
nfl_elo_away <- nfl_elo %>%
  select(
    season, week, 
    qb_name = qb2, qb_elo_value = qb2_value_post, 
    points_for = score2, points_against = score1
  )

flat_df <- bind_rows(nfl_elo_home, nfl_elo_away) %>%
  mutate(
    point_margin = points_for - points_against,
    win = if_else(point_margin > 0, 1, 0)
  )

#Adjusting for era

#538’s QB ratings are based on stats that have increased overtime alongside 
#improved QB play. This can be seen by looking at the median QB Elo 
#value overtime:

## calculate median QB values by season week ##

median_df <- flat_df %>%
  group_by(season, week) %>%
  summarise(
    qb_elo_value_median = median(qb_elo_value),
    qb_elo_value_min = min(qb_elo_value),
    qb_elo_value_max = max(qb_elo_value)
  )

#To make 538’s Elo values comparable across era, this stat inflation needs to 
#be removed:

flat_df <- flat_df %>%
  group_by(season, week) %>%
  mutate(
    qb_elo_value_median = median(qb_elo_value, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    qb_elo_value_era_adjusted = qb_elo_value - qb_elo_value_median
  )

#Part 3: Adding stats and compiling careers

## add weekly ranking ##

flat_df <- flat_df %>%
  group_by(season, week) %>%
  mutate(
    qb_rank = rank(-qb_elo_value),
    top_1_qb = if_else(qb_rank <= 1, 1, 0),
    top_3_qb = if_else(qb_rank <= 3, 1, 0),
    top_5_qb = if_else(qb_rank <= 5, 1, 0)
  ) %>%
  ungroup()


## add cumulative count ##

flat_df %>%
  group_by(qb_name) %>%
  mutate(
    game_number = add_count(qb_name),
    cumulative_era_adjusted_value = cumsum(qb_elo_value_era_adjusted),
    cumulative_wins = cumsum(wins),
    cumulative_best_starter = cumsum(top_1_qb),
    cumulative_top_3_starts = cumsum(top_3_qb),
    cumulative_top_5_starts = cumsum(top_5_qb)
  )
  
#After adding stats, compile at the QB level to get a look at their career:

## aggregate ##
agg_df <- flat_df %>%
  group_by(qb_name) %>%
  summarise(
    total_starts = max(game_number),
    cumulative_era_adjusted_elo_value = sum(qb_elo_value_era_adjusted),
    pct_of_starts_as_best_qb = mean(top_1_qb, na.rm = TRUE),
    pct_of_starts_as_top3_qb = mean(top_3_qb, na.rm = TRUE),
    pct_of_starts_as_top5_qb = mean(top_5_qb, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    average_era_adjusted_elo_value = cumulative_era_adjusted_elo_value / total_starts
  )

#Sort by total Elo value to see the era adjusted rankings:
  
## sort ##

agg_df %>%
  slice_max(cumulative_era_adjusted_elo_value, n = 15)


