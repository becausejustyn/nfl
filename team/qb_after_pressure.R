library(tidyverse)
library(nflfastR)
library(scales)

pbp <- purrr::map_df(c(2016:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

### QB after being pressured

pbp_pressure <- pbp %>%
  filter(pass == 1 | rush == 1) %>%
  filter(season == 2021) %>%
  group_by(game_id, posteam) %>%
  mutate(hit_prev_play = lag(qb_hit))

passer_teams <- pbp %>%
  group_by(passer, posteam) %>%
  summarise(plays = n()) %>%
  top_n(1) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

passers <- pbp_pressure %>%
  filter(!is.na(passer)) %>%
  filter(hit_prev_play == 1) %>%
  group_by(passer) %>%
  summarise(
    pres_passes = n(),
    ypa = mean(air_yards, na.rm = TRUE),
    epa = mean(epa, na.rm = TRUE)
  ) %>%
  filter(pres_passes >= 10) %>%
  left_join(passer_teams, by = c("passer"))

p2 <- passers %>%
  ggplot(aes(
    x = epa, 
    y = ypa)) +
  geom_point(aes(
    size = pres_passes, 
    fill = team_color, 
    color = team_color2), shape = 21) +
  ggrepel::geom_text_repel(aes(label = passer), size = 4, box.padding = 0.20) +
  becausejustynfun::white_theme() +
  geom_hline(aes(yintercept = mean(ypa)), linetype = "dashed", alpha = 0.5) +
  geom_vline(aes(xintercept = mean(epa)), linetype = "dashed", alpha = 0.5) +
  scale_size(name = "Passes After Pressure") +
  scale_color_identity(aesthetics = c("fill", "color")) +
  labs(
    x = "EPA After Pressure", 
    y = "Yard Per Attempt After Pressured",
    title = "How Quarterbacks Perform on Plays After Being Pressured",
    subtitle = "2016 - 2021, minimum of 10 passes after being pressured, bubble size is sample size",
    caption = "@becausejustyn"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave(p2, path = "plots", filename = "qb_after_pressure.png", dpi = 600)