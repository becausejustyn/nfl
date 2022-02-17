### Quarterback Bad Throw Percentage

library(ggthemes)
library(ggrepel)
library(scales)
library(rvest)
library(dplyr)

# scrape data from PFR
url <- "https://www.pro-football-reference.com/years/2021/passing_advanced.htm"
pfr_raw <- url %>%
  read_html() %>%
  html_table() %>%
  as.data.frame()

# clean the scraped data
colnames(pfr_raw) <- make.names(pfr_raw[1,], unique = TRUE, allow_ = TRUE)

pfr <- pfr_raw %>%
  slice(-1) %>%
  select(Player, team = Tm, IAY.PA, Bad., Att) %>%
  mutate(
    Player = stringr::str_replace(Player, "\\*", ""),
    Player = stringr::str_replace(Player, "\\+", ""),
    IAY.PA = as.numeric(IAY.PA),
    Bad. = as.numeric(stringr::str_replace(Bad., "%", "")),
    Passattempts = as.numeric(Att)
  ) %>%
  mutate(team = stringr::str_replace_all(team,
                                c(
                                  "NOR" = "NO",
                                  "SFO" = "SF",
                                  "KAN" = "KC",
                                  "TAM" = "TB",
                                  "NWE" = "NE",
                                  "GNB" = "GB"
                                ))) %>%
  left_join(select(nflfastR::teams_colors_logos, team = team_abbr, contains('_color'))) %>%
  filter(Passattempts > 180) %>%
  arrange(Bad.)

# create the plot
p1 <- pfr %>%
  ggplot(aes(
    x = IAY.PA, 
    y = Bad. / 100)) +
  geom_hline(aes(yintercept = mean(Bad. / 100)), color = "red", linetype = "dotted") +
  geom_vline(aes(xintercept = mean(IAY.PA)), color = "red", linetype = "dotted") +
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.3) +
  geom_point(aes(cex = Passattempts, colour = team_color), alpha = 0.6) +
  geom_text_repel(aes(label = Player), force = 1, point.padding = 0, segment.size = 0.1) +
  scale_y_continuous(breaks = seq(0.1, 0.25, 0.025), labels = percent) +
  scale_size_area(max_size = 8) +
  labs(
    x = "Average Depth of Target in Yards",
    y = "Bad Throw Percentage",
    caption = "Bad% = Percentage of throws that weren't catchable with normal effort, excluding spikes and throwaways\nData: @pfref",
    title = "QB Passing Performance 2021"
  ) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  theme_stata() +
  theme(
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    legend.position = "right"
  )

ggsave(p1, path = "plots", filename = "qb_bad_throw_adot.png", dpi = 600)