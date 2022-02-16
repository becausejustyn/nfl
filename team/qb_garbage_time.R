library(tidyverse)
library(nflfastR)
library(scales)

pbp <- purrr::map_df(c(2016:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

### QB Garbage Time

pbp1 <- pbp %>%
  filter(!is.na(epa)) %>%
  filter(!is.na(passer))

non_gb_time <- pbp1 %>%
  filter(qb_dropback == 1) %>%
  filter(wp > 0.05 & wp < 0.95) %>%
  group_by(passer) %>%
  summarise(
    non_gb_passes = n(), 
    epa_per_db = mean(epa)
  )

gb_time <- pbp1 %>%
  filter(qb_dropback == 1) %>%
  filter(wp < 0.05 | wp > 0.95) %>%
  group_by(passer) %>%
  summarise(
    gb_passes = n(), 
    gb_epa = mean(epa)
  )

player_teams <- pbp1 %>%
  group_by(passer, posteam) %>%
  summarise(snaps = n()) %>%
  top_n(1) %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr"))

all_time <- non_gb_time  %>%
  left_join(gb_time, by = c("passer")) %>%
  left_join(player_teams, by = c("passer")) %>%
  arrange(desc(snaps)) %>%
  slice_head(n = 40)

p1 <- all_time %>%
  ggplot() +
  geom_smooth(aes(
    x = epa_per_db, 
    y = gb_epa), 
    method = "lm", colour = "grey", se = FALSE) +
  ggrepel::geom_text_repel(aes(
    x = epa_per_db, 
    y = gb_epa, 
    label = passer), 
    box.padding = 0.3, size = 5) +
  geom_point(aes(
    x = epa_per_db, 
    y = gb_epa, 
    size = snaps, 
    fill = team_color, 
    color = team_color2), shape = 21) +
  geom_hline(yintercept = mean(all_time$gb_epa), colour = "black", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(all_time$epa_per_db), colour = "black", linetype = "dashed", alpha = 0.5) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  scale_size(name = "Dropbacks") +
  becausejustynfun::white_theme() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = "Non-Garbage Time EPA/dropback", 
    y = "Garbage Time EPA/dropback",
    title = "QB EPA Differences In Garbage Time",
    subtitle = "Min 600 Dropbacks, Garbage Time is WP >= 95% or <= 5%"
  )

ggsave(p1, path = "plots", filename = "qb_garbage_time.png", dpi = 600)