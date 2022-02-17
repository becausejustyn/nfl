library(tidyverse)

## PBP, Roster & Snap Count Data

pbp <- purrr::map_df(2021, function(x) {
  read_rds(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

rosters <- purrr::map_df(2021, function(x) {
  read_rds(
    glue::glue("~/Documents/nfl/data/roster/roster_{x}.rds")
  )
})

#Snap Counts
snap_count_2021 <- purrr::map_df(2021, function(x) {
  read_csv(
    glue::glue("~/Documents/nfl/data/snap_count/snap_count_{x}.csv")
  )
})

snap_count_2021_df <- snap_count_2021 %>%
  filter(game_type == 'REG') %>%
  select(pfr_player_id, week, game_id, ends_with(c("snaps", "pct"))) %>%
  left_join(filter(rosters, season == 2021), by = c("pfr_player_id" = "pfr_id")) %>%
  filter(team == "PHI", position %in% c("RB", "WR", "TE")) %>%
  select(full_name, position, week, offense_snaps, offense_pct, defense_snaps, defense_pct)

phi_colours <- nflfastR::teams_colors_logos %>%
  filter(team_abbr == 'PHI')


p1 <- snap_count_2021_df %>%
  filter(!full_name %in% c("Noah Togiai", "Jason Huntley", "Richard Rodgers", "John Hightower")) %>%
  ggplot(aes(
    x = week, 
    y = offense_pct
  )) +
  geom_col(aes(fill = phi_colours$team_color, colour = phi_colours$team_color2)) +
  scale_x_continuous(breaks = seq(1, 18, 3)) +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  labs(
    x = '',
    y = 'Offense Snap Percent',
    title = 'Eagles Offense Snap Percent',
    subtitle = 'Regular Season, 2021',
    caption = 'data: nflfastr'
  ) +
  tomtom::theme_538() + #Tom Mock theme
  facet_wrap(vars(full_name))

ggsave(p1, path = "plots", filename = "eagles_snap_counts.png", dpi = 600)