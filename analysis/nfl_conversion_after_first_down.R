library(nflfastR)
library(tidyverse)
library(becausejustynfun)

pbp <- purrr::map_df(c(2011:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
}) 

first_down <- pbp %>% 
  filter(
    !is.na(down),
    down == 1
    ) %>%
  select(down, yards_gained, series, series_result) %>% 
  group_by(yards_gained, series_result) %>% 
  summarise(
    count = n(),
    .groups = "drop"
  ) %>% 
  filter(!series_result %in% c('End of half', 'QB kneel')) %>%
  mutate(
    type = ifelse(series_result %in% c('Punt','Turnover on downs', 'Safety','Turnover', 'Opp touchdown'), 'Stop','Not')
  ) %>% 
  group_by(yards_gained, type) %>% 
  summarise(
    num = sum(count),
    .groups = "drop"
  ) %>% 
  group_by(yards_gained) %>% 
  mutate(
    tot_num = sum(num)
  ) %>% 
  ungroup() %>% 
  mutate(
    rate = num/tot_num
  ) %>%
  filter(
    type == 'Stop', 
    tot_num >= 10,
    yards_gained < 10
    ) 


first_down %>% 
  ggplot() +
  geom_point(mapping = aes(x = yards_gained, y = rate * 100)) +
  geom_smooth(mapping = aes(x = yards_gained, y = rate * 100), method = 'lm', se = FALSE) +
  # ylim(0,100)+
  scale_x_continuous(breaks = seq(-13, 9)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  #white_theme() +
  theme(axis.title = element_text()) +
  labs(
    x = 'Yards Gained on First Down',
    y = 'Series Stop Rate (%)',
    title = 'How Important is Limiting Yards on 1st Down?',
    subtitle = 'Stop = Punt, Turnover on Downs, Safety, Turnover | 2021 | min 10 occurrences',
    caption = 'Data: nflfastR'
  )
