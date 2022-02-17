library(tidyverse)
library(nflfastR)
library(scales)

pbp <- purrr::map_df(c(2016:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
})

qbs <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarise(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = TRUE),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 500) %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

p1 <- qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  geom_point(color = qbs$team_color, cex=qbs$n_plays / 350, alpha = .6) +
  geom_text_repel(aes(label=name)) +
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2016 - 2021",
       caption = "Data: @nflfastR") +
  theme_bw() + 
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

ggsave(p2, path = "plots", filename = "qb_efficiency.png", dpi = 600)