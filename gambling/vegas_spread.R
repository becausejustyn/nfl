library(tidyverse)

pbp <- read_rds("~/pbp_2021.rds")

pbp %>%
  mutate(
    dist_line = score_differential - spread_line
  ) %>%
  filter(!is.na(dist_line)) %>%
  ggplot(aes(x = dist_line)) +
  geom_histogram(aes(y = ..density..), bins = 30L) +
  stat_function(fun = dnorm,
                geom = "line",
                size = 2,
                colour = "steelblue",
                args = list(
                  mean = -3.020135,
                  sd = 12.93957
                )) +
  scale_x_continuous(breaks = seq(-60, 60, 10)) +
  labs(
    y = "Frequency",
    x = "Distance from Actual Spread",
    caption = "data: nflfastR"
  )