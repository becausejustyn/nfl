

library(tidyverse)
library(ggtext) 
library(glue)
library(nflfastR)

pbp <- read_rds("~/Documents/nfl/data/pbp/play_by_play_2021.rds")

df <- pbp %>%
  filter(season_type == 'REG') %>%
  group_by(game_id, home_team, away_team) %>%
  select(result:total_line) %>%
  distinct(game_id, .keep_all = TRUE) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(home_team, away_team),
    values_to = "team",
    names_to = "home_away"
  ) %>%
  mutate(
    across(game_id, stringr::str_replace, "2021_", ""),
    week = substr(game_id, 1, 2),
    week = as.numeric(week)
    ) %>%
  left_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr"))

df %>%
  mutate(name = glue::glue("<i style='color:{team_color}'>{team}</i>")) %>%
  ggplot(aes(
    x = week, 
    y = spread_line, 
    group = 1, 
    colour = team_color)) +
  geom_line() +
  scale_color_identity(aesthetics = c('fill', 'colour')) + 
  scale_x_continuous(breaks = seq(1, 18, 2)) +
  facet_wrap(vars(name), scales = "free_x") +
  becausejustynfun::white_theme() +
  theme(
    strip.text = element_markdown()
  ) +
  labs(
    x = "Week",
    y = "Spread",
    caption = "data: nflfastR"
  )
  
df %>%
  mutate(name = glue::glue("<i style='color:{team_color}'>{team}</i>")) %>%
  ggplot(aes(
    x = week, 
    y = spread_line, 
    group = 1, 
    colour = team_color)) +
  geom_line() +
  geom_hline(aes(yintercept = 0, colour = team_color2), linetype = "dashed") +
  scale_color_identity(aesthetics = c('fill', 'colour')) + 
  scale_x_continuous(breaks = seq(1, 18, 2)) +
  facet_wrap(vars(name), scales = "free_x", ncol = 5) +
  becausejustynfun::white_theme() +
  theme(
    strip.text = element_markdown()
  ) +
  labs(
    x = "Week",
    y = "Spread",
    caption = "data: nflfastR"
  )
  
df %>%
  ggplot(aes(
    x = week, 
    y = spread_line, 
    group = 1)) +
  geom_line()