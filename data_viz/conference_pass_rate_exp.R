library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)

pbp <- read_rds("~/Documents/nfl/data/pbp/play_by_play_2021.rds") %>%
  filter(season_type == 'REG') %>%
  filter(
    special == 0 & 
      special_teams_play == 0 & 
      is.na(st_play_type) & 
      !play_type %in% NA
  )

pbp_rp <- pbp %>%
  filter(pass == 1 | rush == 1) 

proe_season <- pbp_rp %>%
  group_by(posteam, week) %>%
  summarise(
    proe = mean(pass_oe, na.rm = TRUE)
  ) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  mutate(
    division = case_when(
      posteam %in% c("GB", "MIN", "DET", "CHI") ~ "NFC North",
      posteam %in% c("ATL", "CAR", "NO", "TB") ~ "NFC South",
      posteam %in% c("DAL", "NYG", "PHI", "WAS") ~ "NFC East",
      posteam %in% c("ARI", "LA", "SEA", "SF") ~ "NFC West",
      posteam %in% c("BAL", "CIN", "CLE", "PIT") ~ "AFC North",
      posteam %in% c("HOU", "IND", "JAX", "TEN") ~ "AFC South",
      posteam %in% c("BUF", "MIA", "NE", "NYJ") ~ "AFC East",
      posteam %in% c("DEN", "KC", "LV", "LAC") ~ "AFC West"
    ),
    conference = case_when(
      division %in% c("AFC North", "AFC South", "AFC East", "AFC West") ~ "AFC",
      TRUE ~ "NFC"
    )
  ) %>%
  mutate(
    division = as_factor(division),
    conference = as_factor(conference)
  )

## NFC Pass Rate Over Expected

p1 <- proe_season %>%
  filter(conference == "NFC") %>%
  ggplot(aes(x = week, y = proe)) +
  geom_bar(aes(
    fill = team_color, color = team_color2), 
    stat = "identity", alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(.~division + posteam) +  
  scale_x_continuous(breaks = seq(1, 18, 2)) +
  tomtom::theme_538() +
  labs(
    x = "Week",
    y = "Pass Rate Over Expected",
    title = "Pass Rate Over Expected NFC, 2021",
    caption = "data: nflfastr | @becausejustyn"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text = element_text(size = 13),
    panel.grid.major.x = element_line(size = 0.1),
    strip.text.x = element_text(size = 14)
  )

ggsave(p1, path = "plots", filename = "nfc_pass_rate_o_exp.png", dpi = 600)



## AFC Pass Rate Over Expected

p2 <- proe_season %>%
  filter(conference == "AFC") %>%
  ggplot(aes(x = week, y = proe)) +
  geom_bar(aes(
    fill = team_color, color = team_color2), 
    stat = "identity", alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(.~division + posteam) +  
  scale_x_continuous(breaks = seq(1, 18, 2)) +
  tomtom::theme_538() +
  labs(
    x = "Week",
    y = "Pass Rate Over Expected",
    title = "Pass Rate Over Expected AFC, 2021",
    caption = "data: nflfastr | @becausejustyn"
  ) +
  theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text = element_text(size = 13),
    panel.grid.major.x = element_line(size = 0.1),
    strip.text.x = element_text(size = 14)
  )

ggsave(p2, path = "plots", filename = "afc_pass_rate_o_exp.png", dpi = 600)