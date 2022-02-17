

library(tidyverse)

pbp <- read_rds("~/Documents/nfl/data/pbp/play_by_play_2021.rds") %>%
  filter(season_type == 'REG')

df <- pbp %>%
  #filter(is.na(play_type)) %>%
  filter(
    !is.na(down) & 
      !is.na(play_type) & 
      !is.na(penalty)) %>%
  group_by(game_id, posteam) %>%
  summarise(
    snaps = sum(play)
  ) %>%
  group_by(posteam) %>%
  summarise(
    snaps = sum(snaps)
  ) %>%
  left_join(
    nflfastR::teams_colors_logos %>% 
      select(team_abbr, team_color, team_color2, team_color3, team_color4), 
    by = c("posteam" = "team_abbr")) %>%
  mutate(
    team_fact = as_factor(posteam),
    team_fact = fct_reorder(team_fact, -snaps)
  )

p1 <- df %>%
  ggplot() +
  geom_col(
    aes(x = snaps, y = fct_reorder(posteam, snaps),
        fill = team_color, colour = team_color2), width = 0.9, size = 0.5
  ) +
  coord_cartesian(xlim = c(1000, 1250)) +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  hrbrthemes::theme_ft_rc(base_size = 9.5, axis_title_size = 11) +
  labs(
    y = 'Offensive Snaps',
    x = 'Team',
    title = 'Offensive Snaps by Team',
    subtitle = '2021, Regular Season',
    caption = "data: nflfastR"
  )

ggsave(p1, path = "plots", filename = "off_snaps_team1.png", dpi = 600)

## Different Version
## Has the number of snaps for each team in the bar

p2 <- df %>%
  ggplot() +
  geom_col(
    aes(x = snaps, y = fct_reorder(posteam, snaps),
        fill = team_color, colour = team_color2), alpha = 0.6, width = 0.9, size = 0.5
  ) +
  geom_text(
    aes(x = snaps, y = fct_reorder(posteam, snaps), label = snaps, colour = team_color2), 
    hjust = 1, stat = 'unique') +
  coord_cartesian(xlim = c(950, 1250)) +
  scale_x_continuous(breaks = seq(950, 1250, 25)) +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  hrbrthemes::theme_ft_rc() +
  labs(
    y = 'Offensive Snaps',
    x = 'Team',
    title = 'Offensive Snaps by Team',
    subtitle = '2021, Regular Season',
    caption = "data: nflfastR"
  )

ggsave(p2, path = "plots", filename = "off_snaps_team2.png", dpi = 600)


## Differences Between Highest and Lowest Snap Count by Team
## Range

#getting weekly snap count
play_count_weekly <- pbp %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam, week) %>%
  summarise(
    play_count = sum(pass_attempt, rush_attempt, sack, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  left_join(
    nflfastR::teams_colors_logos %>% 
      select(team_abbr, team_name, team_color, team_color2, team_color3, team_color4), 
    by = c("posteam" = "team_abbr"))

#getting the min and max for each group
play_count_weekly %>%
  group_by(posteam) %>% 
  filter(play_count %in% c(min(play_count), max(play_count))) %>%
  mutate(play_range = max(play_count) - min(play_count)) %>%
  ungroup() 

#if there are ties it can create issues, so this is another option
play_count_weekly %>%
  group_by(posteam) %>%
  arrange(play_count) %>% 
  slice(c(1, n())) %>%
  mutate(play_range = max(play_count) - min(play_count)) %>%
  ungroup()

#returns the highest and lowest. just to get an idea for what scale limits 
play_count_weekly %>%
  group_by(posteam) %>%
  arrange(play_count) %>% 
  slice(c(1, n())) %>%
  mutate(play_range = max(play_count) - min(play_count)) %>%
  distinct(play_range, .keep_all = TRUE) %>%
  ungroup() %>%
  arrange(play_range) %>% 
  slice(c(1, n()))

p3 <- play_count_weekly %>%
  group_by(posteam) %>%
  arrange(play_count) %>% 
  slice(c(1, n())) %>%
  mutate(play_range = max(play_count) - min(play_count)) %>%
  distinct(posteam, .keep_all = TRUE) %>%
  ungroup() %>%
  ggplot(aes(
    x = reorder(posteam, play_range), 
    weight = play_range, 
    colour = team_color2, fill = team_color)) +
  geom_bar() +
  coord_flip(ylim = c(15, 50)) + 
  scale_y_continuous(breaks = seq(15, 50, 5)) +
  geom_text(
    aes(y = play_range, x = fct_reorder(posteam, play_range), label = play_range, colour = team_color2), 
    hjust = 1.25, stat = 'unique') +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  tomtom::theme_538() +
  labs(
    x = '', y = '',
    title = 'Range In Offensive Plays',
    subtitle = "Regular Season, 2021",
    caption = "data: nflfastR"
  )

ggsave(p3, path = "plots", filename = "off_snaps_team_range.png", dpi = 600)