library(tidyverse)
library(nflfastR)

rosters <- purrr::map_df(c(2020, 2021), function(x) {
  readr::read_rds(
    glue::glue("~/Documents/nfl/data/roster/roster_{x}.rds")
  )
}) %>%
  filter(
    position %in% c("QB", "WR", "RB", "FB", "TE")
    ) %>%
  select(
    position, season, posteam = team,
    receiver_jersey_number = jersey_number
  )

pbp <- purrr::map_df(c(2020, 2021), function(x) {
  readr::read_rds(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

data_clean <- pbp %>%
  filter(
    pass == 1,
    sack == 0,
    qb_scramble == 0,
    !is.na(receiver_jersey_number)
    ) %>%
  filter(season_type == 'REG') %>%  #week <= 17
  select(
    season, name, pass, desc, posteam, epa, defteam, complete_pass, incomplete_pass,
    air_yards, receiver_player_name, receiver_jersey_number, down, success, complete_pass
  ) %>%
  left_join(rosters, by = c("receiver_jersey_number", "posteam", "season")) %>%
  filter(!is.na(position)) %>%
  mutate(position = if_else(position == "FB", "RB", position))

pos <- data_clean %>%
  filter(position == "RB")

season_stats <- pos %>%
  filter(!is.na(success)) %>%
  select(posteam, success, season) %>%
  group_by(season) %>%
  add_count(posteam) %>%
  count(posteam, success) %>%
  mutate(success = if_else(success == 0, "Attempts", "Successful")) %>%
  pivot_wider(names_from = success, values_from = n) %>%
  mutate(
    Attempts = Successful + Attempts,
    success_rate = Successful/Attempts,
    success_rate = round(success_rate, digits = 3) * 100
  ) %>%
  arrange(desc(success_rate)) %>%
  ungroup()



# 2021 recreation
season_stats %>%
  filter(season == 2021) %>%
  select(-season) %>%
  gt() %>%
  tab_spanner(
    label = "PASSES TO RBS",
    columns = c(Attempts, Successful)
  ) %>%
  tab_header(
    title = md("**The Chiefs got the most out of the running back pass**"),
    subtitle = md("NFL teams by success rate of passes to running backs, as measured by positive<br>expected points added, for the 2021 regular season")
  ) %>%
  data_color(
    columns = c(success_rate),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>%
  cols_label(
    success_rate = "SUCCESS RATE (%)"
  ) %>%
  tab_source_note(
    source_note = md("SOURCE: NFLFASTR<br>TABLE: @me")
  ) %>%
  gt_theme_538(table.width = px(550))

