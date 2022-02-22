
#https://www.covers.com/nfl/home-field-advantage

pbp <- purrr::map_df(c(2001:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
}) %>%
  filter(season_type == 'REG')


pbp %>%
  filter(!is.na(posteam_type)) %>%
  ggplot(aes(x = season, y = mean(spread_line))) +
  geom_line() +
  facet_wrap(~posteam_type)

team_spread <- pbp %>% 
  filter(!is.na(posteam_type)) %>% 
  select(season, home_team, away_team, posteam_type, spread_line) %>%
  group_by(season, home_team, away_team) %>%
  summarise(
    spread_line = mean(spread_line)
  )

#compare spread for home team vs away team
league_spread <- pbp %>% 
  filter(!is.na(posteam_type)) %>% 
  select(season, posteam_type, spread_line) %>%
  group_by(season, posteam_type) %>%
  summarise(
    spread_line = mean(spread_line)
  )

ggplot(pbp441) +
  aes(x = season, y = spread_line, group = home_team) +
  geom_line(size = 0.5, colour = "#112446") +
  theme_minimal()



ggplot(league_spread) +
  aes(x = season, y = spread_line, colour = posteam_type) +
  geom_line(size = 0.5) +
  scale_color_manual(
    values = c(away = "#1B9E77",
               home = "#666666")
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  labs(
    x = "Season",
    y = "Mean Spread",
    color = "Game Location"
  ) +
  theme_minimal()

league_spread %>%
  filter(posteam_type == 'home') %>%
  ggplot(aes(
    x = season, y = spread_line
  )) +
  geom_line()



league_result <- pbp %>% 
  filter(!is.na(posteam_type)) %>% 
  select(season, posteam_type, result) %>%
  group_by(season, posteam_type) %>%
  summarise(
    result = mean(result)
  )

league_result %>%
  filter(posteam_type == 'home') %>%
  ggplot(aes(
    x = season, y = result
  )) +
  geom_line() +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) 

#spread and result
league_total <- pbp %>% 
  filter(!is.na(posteam_type)) %>% 
  select(season, posteam_type, spread_line, result) %>%
  group_by(season, posteam_type) %>%
  summarise(
    spread_line = mean(spread_line),
    result = mean(result)
  )

league_total %>%
  filter(posteam_type == 'home') %>%
  ggplot(aes(x = season)) +
  geom_line(aes(y = spread_line, colour = "spread_line")) +
  geom_line(aes(y = result, colour = "result")) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  scale_colour_manual("", 
                      breaks = c("spread_line", "result"),
                      values = c("#7fcdbb", "#8856a7")) 


league_total %>%
  mutate(diff = spread_line - result)