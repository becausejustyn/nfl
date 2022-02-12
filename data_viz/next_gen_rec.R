### Next Gen Stats Receiving

source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")

# load NGS Dataset
NGS_rec_total <- read_csv("ngs_receiving_2019_overall.csv")

# compute the dataframe for first plot
NGS_total_p1 <- NGS_rec_total %>%
  apply_colors_and_logos() %>%
  select(-X1) %>%
  arrange(desc(shareOfTeamAirYards)) %>%
  filter(row_number() <= 40)

# compute the dataframe for second plot
NGS_total_p2 <- NGS_rec_total %>%
  apply_colors_and_logos() %>%
  select(-X1) %>%
  arrange(desc(avgYACAboveExpectation)) %>%
  filter(row_number() <= 32)

# create the first plot
p1 <-
  NGS_total_p1 %>%
  ggplot(aes(x = shareOfTeamAirYards / 100, y = avgYACAboveExpectation)) +
  geom_point(aes(cex = targets),
             alpha = 0.5,
             color = NGS_total_p1$use_color) +
  geom_vline(aes(xintercept = mean(shareOfTeamAirYards / 100)), 
             color = "red", linetype = "dotted") +
  geom_hline(aes(yintercept = mean(avgYACAboveExpectation)), 
             color = "red", linetype = "dotted") +
  geom_hline(yintercept = 0, size = 0.25) +
  ggrepel::geom_text_repel(aes(label = shortName)) +
  scale_x_continuous(labels = scales::percent, limits = c(NA, NA)) +
  scale_size("Number of Targets") +
  labs(
    x = "Players Share of His Team’s Total Intended Air Yards",
    y = "Average Yards after Catch above Expectation",
    title = "Receiving Performance 2019 Regular Season",
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

# create the second plot
p2 <-
  NGS_total_p2 %>%
  ggplot(aes(x = 1:nrow(NGS_total_p2), y = avgYACAboveExpectation)) +
  geom_col(
    colour = NGS_total_p2$use_color,
    fill = NGS_total_p2$use_color,
    alpha = 0.4,
    width = NGS_total_p2$shareOfTeamAirYards / 60
  ) +
  geom_text(
    aes(label = playerName, y = avgYACAboveExpectation + 0.05),
    angle = 90,
    hjust = 0
  ) +
  scale_size("Number of Targets") +
  labs(
    x = "Rank",
    y = "Average Yards after Catch above Expectation",
    caption = "Figure: @mrcaseb | Data: @NextGenStats",
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