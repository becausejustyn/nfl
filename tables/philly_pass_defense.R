pbp <- purrr::map_df(2021, function(x) {
  read_rds(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
}) %>%
  filter(season_type == 'REG')

pbp_rp <- pbp %>%
  filter(
    !is.na(epa),
    rush == 1 | pass == 1
  ) 

teams <- pbp %>%
  group_by(posteam, pass) %>%
  summarise(epa = mean(epa)) 

philly <- pbp_rp %>%
  filter(posteam == "PHI")

philly_rec <- philly %>%
  filter(!is.na(receiver_player_name)) %>%
  group_by(defteam) %>%
  summarise(
    targets = n(),
    completion_rate = mean(complete_pass),
    adot = mean(air_yards, na.rm = TRUE),
    yac = mean(yards_after_catch, na.rm = TRUE),
    epa = mean(epa)
  ) %>%
  filter(targets >= 10)