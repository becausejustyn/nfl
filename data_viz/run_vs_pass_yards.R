library(tidyverse)
library(nflfastR)
library(scales)

pbp <- load_pbp(seasons = 2021) %>%
  filter(season_type == 'REG')

# run vs pass Yards per play
rush_v_pass <- pbp %>% 
  filter(play_type %in% c("run", "pass"), penalty == 0) %>% 
  group_by(play_type, posteam) %>% 
  summarise(
    avg_yds = mean(yards_gained, na.rm = TRUE),
    .groups = "drop") 

# nfl average
nfl_rvp <- pbp %>% 
  filter(play_type %in% c("run", "pass"), penalty == 0) %>% 
  group_by(play_type) %>% 
  summarise(
    avg_yds = mean(yards_gained, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(
    posteam = "NFL")

# combining 
rush_v_pass <- bind_rows(rush_v_pass, nfl_rvp) %>% 
  mutate(play_type = factor(
    play_type,
    levels = c("pass", "run"),
    labels = c("Pass", "Rush")
  ))

p1 <- ggplot(rush_v_pass) +
  geom_bar(aes(
    x = fct_reorder(posteam, avg_yds), 
    y = avg_yds, 
    fill = play_type),
    stat = "identity", position = "dodge") +
  scale_y_continuous("Average Yards", breaks = seq(3, 8, 1)) +
  scale_x_discrete("Play Type") + 
  scale_fill_manual(values = c("#1f78b4", "#b2df8a")) +
  #scale_fill_manual(values = c("#1b9e77", "#7570b3")) +
  coord_flip(ylim = c(3, 8)) +
  labs(fill = "Play Type") +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0),
    axis.line = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  hrbrthemes::theme_ft_rc() +
  theme(legend.position = "bottom")

ggsave(p1, path = "plots", filename = "average_yards.png", dpi = 600)

# seperating the pass and rush yard columns

rush_v_pass_sep <- rush_v_pass %>%
  pivot_wider(
    names_from = play_type,
    values_from = avg_yds
  )

p2 <- rush_v_pass_sep %>%
  arrange(Pass) %>%
  mutate(posteam = fct_inorder(posteam)) %>%
  ggplot() +
  theme_classic() +
  theme(
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank()) +
  # add a dummy point for scaling purposes
  geom_point(aes(x = 8, y = posteam), size = 0, col = "white") + 
  # add the horizontal posteam lines
  geom_hline(yintercept = 1:32, col = "grey80") +
  # add a point for each male success rate
  geom_point(aes(x = Rush, y = posteam), size = 11, col = "#9DBEBB") +
  # add a point for each female success rate
  geom_point(aes(x = Pass, y = posteam), size = 11, col = "#468189") +
  # add the text (%) for each male success rate
  geom_text(aes(
    x = Rush, y = posteam, label = paste0(round(Rush, 1))),
    col = "black") +
  # add the text (%) for each female success rate
  geom_text(aes(
    x = Pass, y = posteam, label = paste0(round(Pass, 1))),
    col = "white")

ggsave(p2, path = "plots", filename = "average_yards_v2.png", dpi = 600)

# adding team and nfl colours, changing the NJY colours so you can see

rush_v_pass_sep <- rush_v_pass_sep %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  mutate(
    team_color = replace_na(team_color, "#000000"),
    team_color2 = replace_na(team_color2, "#e0e0e0")
  ) %>%
  mutate(across(team_color2, str_replace, "#1c2d25", "#e0e0e0"))

p3 <- rush_v_pass_sep %>% #rush_v_pass_sep
  arrange(Pass) %>%
  mutate(posteam = fct_inorder(posteam)) %>%
  ggplot() +
  # add a dummy point for scaling purposes
  geom_point(aes(x = 8, y = posteam), size = 0, col = "white") + 
  # add the horizontal posteam lines
  geom_hline(yintercept = 1:33, col = "grey80") +
  # add a point for Rush yards
  geom_point(aes(x = Rush, y = posteam, colour = team_color), size = 9) +
  # add a point for Pass yards
  geom_point(aes(x = Pass, y = posteam, colour = team_color2), size = 9) +
  # Rush Text
  geom_text(aes(
    x = Rush, y = posteam, label = paste0(round(Rush, 1)), col = team_color2)) +
  # Pass Text
  geom_text(aes(
    x = Pass, y = posteam, label = paste0(round(Pass, 1)), col = team_color)) +
  scale_y_discrete(expand = expansion(add = 0.75)) +
  scale_x_continuous(breaks = seq(3, 8, 0.5)) +
  scale_color_identity(aesthetics = c("fill", "colour")) +
  labs(y = "", x = 'Yards/Play', caption = 'data: nflfastR') +
  theme(
    axis.ticks = element_line(colour = "black"),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(size = 0)
  ) 

ggsave(p1, path = "plots", filename = "average_yards_v3.png", dpi = 600)

p4 <- rush_v_pass_sep %>%
  arrange(Pass) %>%
  mutate(posteam = fct_inorder(posteam)) %>%
  ggplot() +
  # add a dummy point for scaling purposes
  geom_point(aes(x = 8, y = posteam), size = 0, col = "white") + 
  # add the horizontal posteam lines
  geom_hline(yintercept = 1:33, col = "grey80") +
  # add a point for Rush yards
  geom_point(aes(x = Rush, y = posteam, colour = team_color), size = 9) +
  # add a point for Pass yards
  geom_point(aes(x = Pass, y = posteam, colour = team_color2), size = 9) +
  # Rush Text
  geom_text(aes(
    x = Rush, y = posteam, label = paste0(round(Rush, 1)), col = team_color2)) +
  # Pass Text
  geom_text(aes(
    x = Pass, y = posteam, label = paste0(round(Pass, 1)), col = team_color)) +
  scale_y_discrete(expand = expansion(add = 0.75)) +
  scale_x_continuous(breaks = seq(3, 8, 0.5)) +
  scale_color_identity(aesthetics = c("fill", "colour")) +
  labs(y = "", x = 'Yards/Play', caption = 'data: nflfastR') +
  theme(
    axis.ticks = element_line(colour = "black"),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_line(colour = NA),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(size = 0)
  ) +
  tomtom::theme_538()

ggsave(p4, path = "plots", filename = "average_yards_v4.png", dpi = 600)