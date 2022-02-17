
# Snap Count
## Player by Season

library(tidyverse)

snap_count_2021 <- read_csv("~/Documents/nfl/data/snap_count/snap_count_2021.csv")

snap_df <- snap_count_2021 %>%
  left_join(
    nflfastR::teams_colors_logos %>% 
      select(team_abbr, team_color, team_color2), 
    by = c("team" = "team_abbr")) %>%
  filter(
    position %in% c('WR', 'TE') &
      offense_snaps > 2 &
      game_type == 'REG'
  ) %>%
  group_by(pfr_player_id, player, team, team_color, team_color2, position) %>%
  summarise(
    snaps = sum(offense_snaps)
  ) %>% 
  ungroup() %>%
  mutate(team_factor = as_factor(team))

p1 <- snap_df %>%
  slice_max(snaps, n = 25) %>%
  mutate(
    rank = as.integer(rank(-snaps))) %>%
  ggplot() +
  geom_col(
    aes(x = snaps, y = fct_reorder(player, snaps),
        fill = team_color, colour = team_color2), width = 0.9, size = 0.5
  ) +
  coord_cartesian(xlim = c(850, 1025)) +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  hrbrthemes::theme_ft_rc(base_size = 9.5, axis_title_size = 11) +
  labs(
    x = 'Offensive Snaps',
    y = 'Player',
    title = 'Offensive Snaps by Player',
    subtitle = '2021, Regular Season',
    caption = "data: nflfastR"
  )

ggsave(p1, path = "plots", filename = "off_snaps_player.png", dpi = 600)


## Player by Game

#This I can filter by at least 15 snaps or something

snap_game <- snap_count_2021 %>%
  left_join(
    nflfastR::teams_colors_logos %>% 
      select(team_abbr, team_color, team_color2, team_color3, team_color4), 
    by = c("team" = "team_abbr")) %>%
  filter(
    position %in% c('WR', 'TE') &
      offense_snaps > 2 &
      game_type == 'REG'
  ) %>%
  group_by(pfr_player_id, player, team, opponent, week, game_id, pfr_game_id, team_color, team_color2, team_color3, team_color4) %>%
  summarise(
    snaps = sum(offense_snaps)
  ) %>% 
  ungroup() %>%
  mutate(team_factor = as_factor(team))


p2 <- snap_game %>%
  mutate(
    across("team_color2", str_replace, "#c60c30", "#b0b7bc"),
    game_label = paste0("Week ", week, ", ", team, " vs. ", opponent)
  ) %>%
  arrange(-snaps) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 15) %>%
  ggplot() +
  geom_col(
    aes(x = snaps, y = fct_reorder(player, snaps), fill = team_color, colour = team_color3), 
    width = 0.9, size = 0.5) + 
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_text(
    aes(x = snaps, y = fct_reorder(player, snaps), label = game_label, colour = team_color2), 
    hjust = 1, nudge_x = -.5, size = 4) +
  geom_text(
    aes(x = 50, y = player, label = snaps, colour = team_color2), 
    hjust = 1) +
  coord_cartesian(xlim = c(50, 90)) + 
  hrbrthemes::theme_ft_rc(base_size = 9.5, axis_title_size = 11) +
  labs(
    x = 'Offensive Snaps',
    y = 'Player',
    title = 'Offensive Snaps by Player in a Single Game',
    subtitle = 'Top 15 Games 2021, Regular Season',
    caption = 'caption = "data: nflfastR"'
  )

ggsave(p2, path = "plots", filename = "off_snaps_player_single_game.png", dpi = 600)