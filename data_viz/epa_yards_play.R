
library(tidyverse)
library(becausejustynfun)
library(ggrepel)
library(nflfastR)


pbp <- purrr::map_df(c(2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

rosters <- purrr::map_df(c(2016:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/roster/roster_{x}.rds")
  )
})


epa_play <- pbp %>% 
  filter(pass == 1, !is.na(posteam)) %>% 
  group_by(posteam) %>% 
  summarise(
    n = n(),
    epa_per_db = sum(epa, na.rm = TRUE) / n,
    success_rate = sum(epa, na.rm = TRUE) / n
  )

## EPA Dropback

p1 <- epa_play %>%
  ggplot(aes(x = epa_per_db, y = reorder(posteam, epa_per_db))) +
  geom_col(aes(fill = if_else(epa_per_db >= 0, "#2c7bb6", "#d7181c"))) +
  scale_fill_identity() +
  white_theme() +
  theme(panel.grid.major.y = element_blank()) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  labs(
    x = "",
    y = "EPA per Dropback",
    title = "The majority of teams had positive EPA/dropback",
    subtitle = "But there are some clear outliers",
    caption = "Data: @nflfastR | Plot: @becausejustyn")

ggsave(p1, path = "plots", filename = "epa_dropback.png", dpi = 600)


p2 <- epa_play %>%
  ggplot(aes(x = epa_per_db, y = reorder(posteam, epa_per_db))) +
  geom_col(aes(fill = if_else(epa_per_db >= 0, "#2c7bb6", "#d7181c"))) +
  geom_text(aes(
    label = posteam,
    color = if_else(epa_per_db >= 0, "#2c7bb6", "#d7181c"),
    hjust = if_else(epa_per_db > 0, -0.1, 1.1)
  ),
  fontface = "bold"
  ) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  white_theme() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank()
  ) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = seq(-0.2, 0.3, 0.1)) +
  labs(
    x = "",
    y = "EPA per Dropback",
    title = "The majority of teams had positive EPA/dropback",
    subtitle = "But there are some clear outliers",
    caption = "Data: @nflfastR | Plot: @becausejustyn"
  )

ggsave(p2, path = "plots", filename = "epa_dropback_v2.png", dpi = 600)


## EPA/Dropback by Play Type 

epa_pbp <- pbp %>%
  filter(pass == 1 | rush == 1, !is.na(posteam)) %>% 
  group_by(posteam, pass, rush) %>% 
  summarise(
    n = n(),
    epa_per_db = sum(epa, na.rm = TRUE) / n,
    success_rate = sum(epa, na.rm = TRUE) / n
  ) %>%
  mutate(
    play_type = if_else(pass == 1, "pass", "rush")
  ) %>%
  ungroup() %>%
  select(-c(pass, rush))

epa_pbp %>% 
  ggplot(aes(
    x = fct_rev(fct_reorder2(posteam, desc(play_type), epa_per_db)), 
    y = epa_per_db, color = play_type)) +
  geom_line(aes(group = posteam), color = "grey", size = 3) +
  geom_point(size = 5) +
  scale_color_manual(values = c("#003399", "#ff2b4f")) +
  coord_flip() +
  white_theme() +
  labs(
    x = "", y = "",
    title = "EPA Per Play",
    colour = "Play Type"
  )


## Yards/Dropback by Play Type

rush_v_pass <- pbp %>% 
  filter(play_type %in% c("run", "pass"), penalty == 0) %>% 
  group_by(play_type, posteam) %>% 
  summarize(avg_yds = mean(yards_gained, na.rm = TRUE)) %>% 
  ungroup()

nfl_rvp <- pbp %>% 
  filter(play_type %in% c("run", "pass"), penalty == 0) %>% 
  group_by(play_type) %>% 
  summarize(avg_yds = mean(yards_gained, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(posteam = "NFL")

rush_v_pass <- bind_rows(rush_v_pass, nfl_rvp) %>% 
  mutate(play_type = factor(play_type,
                            levels = c("pass", "run"),
                            labels = c("Pass", "Rush")))



p3 <- rush_v_pass %>% 
  ggplot(aes(
    x = fct_rev(fct_reorder2(posteam, desc(play_type), avg_yds)), 
    y = avg_yds, color = play_type)) +
  geom_line(aes(group = posteam), color = "grey", size = 3) +
  geom_point(size = 5) +
  #coord_flip()
  geom_text(
    data = filter(rush_v_pass, posteam == "NFL" & play_type == "Pass"),
    aes(label = play_type),
    hjust = 0, nudge_y = 0.2, fontface = "bold", size = 6
  ) +
  geom_text(
    data = filter(rush_v_pass, posteam == "NFL" & play_type == "Rush"),
    aes(label = play_type),
    hjust = 1, nudge_y = -0.2, fontface = "bold", size = 6
  ) +
  coord_flip() +
  scale_color_manual(values = c("#003399", "#ff2b4f")) +
  white_theme() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    axis.text.y = element_text(color = if_else(rush_v_pass$posteam == "NFL", "red", "black"))
  ) +
  labs(
    x = "",
    y = "\n Average Yards Gained",
    title = "Passing yards per play outperforms Rushing for all teams",
    caption = "Data: @nflfastR | Plot: @becausejustyn"
  ) +
  scale_y_continuous(
    limits = c(3, 9),
    breaks = seq(3, 8, 1)
  )


ggsave(p3, path = "plots", filename = "yards_dropback.png", dpi = 600)


## Team Yard Differential by Play Type

rush_v_pass_wide <- rush_v_pass %>%
  pivot_wider(names_from = play_type, values_from = avg_yds) %>%
  mutate(Diff = Pass - Rush) %>%
  left_join(select(teams_colors_logos, team_abbr, team_color, team_color2), by = c("posteam" = "team_abbr")) %>%
  #since NFL doesn't have a colour, make it grey
  replace_na(list(
    team_color = "grey", 
    team_color2 = "grey"))

p4 <- rush_v_pass_wide %>%
  ggplot(aes(
    x = Diff, 
    y = reorder(posteam, Diff),
    fill = team_color, colour = team_color2)) +
  geom_col() +
  scale_x_continuous(breaks = seq(0, 3.5, 0.5)) +
  scale_fill_identity(aesthetics = c("fill", "colour")) +
  white_theme() +
  labs(
    y = '', x = '',
    title = 'Yard Differential Between Pass and Rush Plays',
    subtitle = 'Regular Season, 2021',
    caption = 'A greater differential means a team averaged more pass yards per play than rush'
  )

ggsave(p4, path = "plots", filename = "yards_diff_play_type.png", dpi = 600)