

library(tidyverse)
library(becausejustynfun)

pbp <- read_rds("~/Documents/nfl/data/pbp/play_by_play_2021.rds") %>%
  filter(season_type == 'REG')

df <- pbp %>%
  #filter(is.na(play_type)) %>%
  filter(
    !is.na(down), 
    !is.na(play_type), 
    !is.na(penalty),
    play_type %in% c('run', 'pass')) %>%
  group_by(game_id, posteam, play_type) %>%
  summarise(
    snaps = sum(play)
  ) %>%
  group_by(posteam, play_type) %>%
  summarise(
    snaps = sum(snaps)
  ) %>%
  left_join(
    nflfastR::teams_colors_logos %>% 
      select(team_abbr, team_color, team_color2, team_color3, team_color4), 
    by = c("posteam" = "team_abbr")) %>%
  mutate(
    team_fact = as_factor(posteam),
    team_fact = fct_reorder(team_fact, -snaps),
    play_type_fact = as_factor(play_type)
  )

p1 <- df %>%
  group_by(play_type) %>%
  mutate(
    #play_type_rank = rank(-snaps),
    play_type_rank = if_else(play_type == "run", rank(-snaps), 0)
  ) %>%
  ggplot(aes(
    x = snaps, y = fct_reorder(posteam, play_type_rank))) +
  geom_col(aes(fill = play_type)) +
  scale_fill_manual(
    values = c(pass = "#A6CEE3", run = "#71C0EB")) +
  coord_cartesian(xlim = c(300, 1200)) +
  scale_x_continuous(breaks = seq(300, 1200, 150)) +
  labs(
    title = "Offensive Snaps by Play Type",
    subtitle = "Regular Season, 2021",
    caption = "data: nflfastR",
    fill = "Play Type",
    x = "", y = ""
  ) +
  white_theme()

ggsave(p1, path = "plots", filename = "off_snaps_team_type.png", dpi = 600)



## Differences Between Highest and Lowest Snap Count by Team, Play Type
## Range

#getting weekly snap count
play_count_weekly <- pbp %>%
  filter(!is.na(posteam),
         play_type %in% c("run", "pass")) %>%
  group_by(posteam, week, play_type) %>%
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
  group_by(posteam, play_type) %>%
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
  scale_y_continuous(breaks = seq(15, 55, 5)) +
  geom_text(
    aes(y = play_range, x = fct_reorder(posteam, play_range), label = play_range, colour = team_color2), 
    hjust = 1.25, stat = 'unique') +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  tomtom::theme_538() +
  labs(
    x = '', y = '',
    title = 'Range In Offensive Plays, by Play Type',
    subtitle = "Regular Season, 2021",
    caption = "data: nflfastR"
  ) +
  facet_wrap(~ play_type, scales = "free")

ggsave(p3, path = "plots", filename = "off_snaps_team_range_type.png", dpi = 600)