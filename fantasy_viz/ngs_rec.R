### Next Gen Stats Receiving

nflfastR::teams_colors_logos

# load NGS Dataset
ngs_rec <- read_rds("Documents/nfl/data/ngs/ngs_receiving.rds") %>% filter(season == 2021)

ngs_rec_p1 <- ngs_rec %>%
  filter(week == 0) %>%
  left_join(nflfastR::teams_colors_logos) %>%
  arrange(desc(percent_share_of_intended_air_yards)) %>%
  filter(row_number() <= 40)

# create the first plot
ngs_rec_p1 %>%
  ggplot(aes(
    x = percent_share_of_intended_air_yards / 100, 
    y = avg_yac_above_expectation,
    fill = team_color2,
    colour = team_color)) +
  geom_point(aes(cex = targets), alpha = 0.5) +
  geom_vline(aes(
    xintercept = mean(percent_share_of_intended_air_yards / 100)),
    color = "red", linetype = "dotted") +
  geom_hline(aes(yintercept = mean(avg_yac_above_expectation)),
    color = "red", linetype = "dotted") +
  geom_hline(yintercept = 0, size = 0.25) +
  ggrepel::geom_text_repel(aes(label = player_short_name)) +
  scale_x_continuous(labels = scales::percent, limits = c(NA, NA)) +
  scale_y_continuous(breaks = seq(0, 5, 1)) +
  scale_size("Number of Targets") +
  scale_fill_identity(aesthetics = c('fill', 'colour')) +
  labs(
    x = "Players Share of His Team’s Total Intended Air Yards",
    y = "Average Yards after Catch above Expectation",
    title = "Receiving Performance 2021 Regular Season",
    subtitle = "Next Gen Receiving Stats for Receivers (WR+TE) with Top 40 Target Share"
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 1),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(
      size = 10,
      hjust = 0,
      vjust = 0.5,
      face = "bold"),
    legend.position = "top"
    )
