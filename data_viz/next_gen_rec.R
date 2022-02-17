### Next Gen Stats Receiving

source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")

# load NGS Dataset
ngs_receiving <- read_rds("~/Documents/nfl/data/ngs/ngs_receiving.rds")


# compute the dataframe for first plot
ngs_total_p1 <- ngs_receiving %>%
  filter(week == 0, season == 2021) %>%
  arrange(desc(percent_share_of_intended_air_yards)) %>%
  filter(row_number() <= 40) %>%
  left_join(nflfastR::teams_colors_logos, by = c("team_abbr"))

# compute the dataframe for second plot
ngs_total_p2 <- ngs_receiving %>%
  filter(week == 0, season == 2021) %>%
  arrange(desc(avg_yac_above_expectation)) %>%
  filter(row_number() <= 32) %>%
  left_join(nflfastR::teams_colors_logos, by = c("team_abbr"))

# create the first plot
p1 <- ngs_total_p1 %>%
  ggplot(aes(
    x = percent_share_of_intended_air_yards / 100, 
    y = avg_yac_above_expectation)) +
  geom_point(aes(
    cex = targets, colour = team_color),
    alpha = 0.5) +
  geom_vline(aes(xintercept = mean(percent_share_of_intended_air_yards / 100)), 
             color = "red", linetype = "dotted") +
  geom_hline(aes(yintercept = mean(avg_yac_above_expectation)), 
             color = "red", linetype = "dotted") +
  geom_hline(yintercept = 0, size = 0.25) +
  ggrepel::geom_text_repel(aes(label = player_short_name)) +
  scale_x_continuous(labels = scales::percent, limits = c(NA, NA)) +
  scale_size("Number of Targets") +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  labs(
    x = "Players Share of His Team’s Total Intended Air Yards",
    y = "Average Yards after Catch above Expectation",
    title = "Receiving Performance 2021 Regular Season",
    subtitle = "Next Gen Receiving Stats for Receivers (WR+TE) with Top 40 Target Share"
  ) +
  ggthemes::theme_stata(scheme = "sj", base_size = 10) +
  theme(
    plot.title = element_text(face = 'bold'),
    plot.caption = element_text(hjust = 1),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(
      size = 10,
      hjust = 0,
      vjust = 0.5,
      face = 'bold'
    ),
    legend.position = "top"
  )

ggsave(p1, path = "plots", filename = "rec_yac_air_yard.png", dpi = 600)

# create the second plot
p2 <- ngs_total_p2 %>%
  ggplot(aes(x = 1:nrow(ngs_total_p2), y = avg_yac_above_expectation)) +
  geom_col(aes(
    colour = team_color, fill = team_color, 
    width = percent_share_of_intended_air_yards / 60),
    alpha = 0.4
  ) +
  geom_text(
    aes(label = player_display_name, y = avg_yac_above_expectation + 0.05),
    angle = 90,
    hjust = 0
  ) +
  scale_size("Number of Targets") +
  scale_color_identity(aesthetics = c('fill', 'colour')) +
  labs(
    x = "Rank",
    y = "Average Yards after Catch above Expectation",
    caption = "Data: @NextGenStats",
    subtitle = "Top 32 Receivers (WR+TE) in Average Yards after Catch above Expectation (Column Width Corresponds to Target Share)"
  ) +
  ylim(NA, 5.7) +
  ggthemes::theme_stata(scheme = "sj", base_size = 10) +
  theme(
    plot.title = element_text(face = 'bold'),
    plot.caption = element_text(hjust = 1),
    axis.text.y = element_text(angle = 0, vjust = 0.5),
    legend.title = element_text(
      size = 10,
      hjust = 0,
      vjust = 0.5,
      face = 'bold'
    )
  )

# commbine the plots
plot <- gridExtra::grid.arrange(p1, p2, nrow = 2)