

library(tidyverse)
library(becausejustynfun)
library(ggrepel)
library(nflfastR)

pbp <- read_rds("~/Documents/nfl/data/pbp/play_by_play_2021.rds")
rosters <- read_rds("~/Documents/nfl/data/roster/roster_2021.rds")

## Success Rate vs. EPA of receivers

df <- pbp %>%
  filter(
    !is.na(receiver_player_name) & 
      play_type == "pass" & 
      down <= 4) %>%
  group_by(receiver_player_name, posteam) %>%
  summarise(
    success_rate = mean(success), 
    mean_epa = mean(epa), 
    targets = n()
  ) %>%
  filter(targets >= 100) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr"))
  
p1 <- df %>%
  ggplot(aes(
    x = success_rate, y = mean_epa,
    fill = team_color2, colour = team_color)) + 
  geom_point(aes(size = targets), alpha = 0.6) + 
  geom_text_repel(aes(label = receiver_player_name)) +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  labs(
    x = "Success Rate",
    y = "EPA",
    title = "Receivers: Success Rate and EPA, Regular Season 2021", 
    subtitle = "Minimum 100 Targets",
    caption = "Data: nflfastr",
    size = "Targets"
  ) +
  white_theme()

ggsave(p1, path = "plots", filename = "wr_success_vs_epa.png", dpi = 600)

## aDOT vs. Catch Rate

df2 <- pbp %>%
  filter(
    !is.na(receiver_player_name) & 
      play_type == "pass" & 
      down <= 4) %>%
  group_by(receiver_player_name, posteam) %>%
  summarise(
    adot = mean(air_yards), 
    targets = n(), 
    catch_rate = sum(complete_pass)/targets
  ) %>%
  filter(adot >= 5 & targets >= 100) %>%
  left_join(nflfastR::teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ungroup()


p2 <- df2 %>%
  ggplot(aes(
    x = adot, 
    y = catch_rate)) + 
  geom_point(aes(fill = team_color2, colour = team_color, size = targets), alpha = 0.6) + 
  geom_text_repel(aes(
    label = receiver_player_name), 
    color = "grey40", 
    segment.color = "black", 
    size = 2.5) + 
  geom_smooth(method = lm, se = FALSE, linetype = "dashed", color = "black") + 
  scale_y_continuous(breaks = seq(0.4, 0.85, 0.05)) + 
  scale_x_continuous(breaks = seq(5, 15, 1)) + 
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  white_theme() + 
  labs(
    x = "aDOT", 
    y = "Catch Rate",
    title = "Catch Rate vs aDOT",
    subtitle = "Minimum 100 Targets",
    caption = "Data: nflfastr"
  )

ggsave(p2, path = "plots", filename = "catch_rate_adot.png", dpi = 600)


## Target Share vs Air Yard Share

market_share <- read_rds("~/Documents/nfl/data/player_stats/player_stats_2021.rds")

market_share <- market_share %>%
  filter(!is.na(target_share), !is.na(air_yards_share), targets > 1) %>%
  group_by(player_name, player_id) %>%
  summarise(
    target_share = mean(target_share, na.rm = TRUE),
    air_yards_share = mean(air_yards_share, na.rm = TRUE),
    targets = sum(targets, na.rm = TRUE)
  ) %>%
  left_join(select(market_share, recent_team, player_id)) %>%
  distinct(player_id, .keep_all = TRUE) %>%
  left_join(nflfastR::teams_colors_logos, by = c("recent_team" = "team_abbr"))

p3 <- market_share %>%
  filter(targets > 50) %>%
  ggplot(aes(
    x = target_share, 
    y = air_yards_share, 
    size = targets,
    fill = team_color,
    colour = team_color2)) +
  geom_point(shape = 21, alpha = 0.8) +
  geom_label_repel(aes(label = player_name), fill = "white", size = 3) +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  labs(
    title = "Target Share vs Air Yard Share - 2021 Season",
    subtitle = "Bubble Size = Total Targets (min. 100 targets)",
    caption = "Data: nflfastR",
    y = "Air Yards Share",
    x = "Target Share"
  ) + 
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "light grey"),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18),
    axis.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  white_theme()

ggsave(p3, path = "plots", filename = "target_share_vs_air_yard_share.png", dpi = 600)


## WR Catch Rate by aDoT

#rm(list = ls())

pbp <- purrr::map_df(c(2016:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

rosters <- purrr::map_df(c(2016:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/roster/roster_{x}.rds")
  )
})

data_clean <- pbp %>%
  filter(
    pass == 1 & sack == 0 & qb_scramble == 0, 
    !is.na(receiver_jersey_number)
  ) %>%
  select(
    name, pass, desc, posteam, epa, defteam, complete_pass, incomplete_pass,
    air_yards, receiver_player_name, receiver_jersey_number, down, success, complete_pass
  ) %>%
  left_join(rosters, by = c("receiver_jersey_number" = "jersey_number", "posteam" = "team")) %>% 
  filter(!is.na(position)) %>% 
  mutate(position = if_else(position == "FB", "RB", position))

pos <- data_clean %>%
  filter(position != "QB") %>% 
  mutate(position = factor(position, levels = c("WR", "RB", "TE")))

pass_comp <- pos %>%
  filter(between(air_yards, 1, 25)) %>%
  group_by(position, air_yards) %>%
  summarise(
    n = n(),
    comp_rate = sum(complete_pass, na.rm = TRUE) / n,
    epa = mean(epa, na.rm = TRUE)
  ) %>% 
  na.omit()


p4 <- pass_comp %>%
  filter(position == "WR") %>%
  ggplot(aes(
    x = air_yards, 
    y = comp_rate, 
    fill = position)) +
  geom_point(aes(size = n), shape = 21, stroke = 0.5) +
  geom_smooth(color = "white", method = "loess") +
  geom_hline(yintercept = 0, size = 1, color = "black") +
  geom_vline(xintercept = 20, size = 1, color = "black", linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0.5, size = 1, color = "black", linetype = "dashed", alpha = 0.5) +
  facet_grid(~position) +
  scale_fill_manual(
    values = c("#00b159", "#003399", "#ff2b4f"),
    aesthetics = c("color", "fill")
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Air Yards (Depth of Target)",
    y = "Completion Rate \n",
    title = "Completion rate by Depth of Target on 1st/2nd Down",
    subtitle = "Completion rate generally drops below 50% for passes > 20 air yards",
    caption = "Data: nflfastR",
    size = "N of Passes"
  ) +
  guides(color = "none", fill = "none") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.direction = "vertical",
    legend.position = c(0.1, 0.2),
    legend.background = element_blank(),
    legend.title = element_text(face = "bold")
  ) +
  white_theme()

ggsave(p4, path = "plots", filename = "comp_rate_adot_league.png", dpi = 600)


## aDot vs XYAC EPA

wrs1 <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, receiver_player_name) %>%
  summarise(
    air_wpa = mean(air_wpa, na.rm = TRUE),
    yac_wpa = mean(yac_wpa, na.rm = TRUE),
    adot = mean(air_yards, na.rm = TRUE),
    xyac_epa = mean(xyac_epa, na.rm = TRUE),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_plays > 100) %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

p5 <- wrs1 %>%
  filter(!is.na(receiver_player_name)) %>%
  ggplot(aes(
    x = adot, 
    y = xyac_epa, 
    colour = team_color
  )) +
  geom_point(aes(size = n_plays), alpha = .6) +
  geom_text_repel(aes(label = receiver_player_name)) +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  theme_minimal() +
  labs(
    x = "aDoT",
    y = "xYAC EPA",
    title = "aDot vs xYAC, 2016 - 2021",
    caption = "Data: nflfastR",
    size = "n Plays"
  ) +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  white_theme()

ggsave(p5, path = "plots", filename = "adot_vs_xyac.png", dpi = 600)