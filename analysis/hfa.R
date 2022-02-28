
library(tidyverse)

pbp <- purrr::map_df(c(2001:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )}) %>%
  filter(
    season_type == 'REG',
    !is.na(posteam_type)
    ) %>%
  select(game_id, season, week, posteam_type, season_type, home_team, away_team, home_score, away_score) %>%
  group_by(game_id) %>%
  distinct(.keep_all = TRUE)

  
# league wide
league_wide_win <- pbp %>%
  distinct(game_id, .keep_all = TRUE) %>%
  mutate(score_diff = home_score - away_score) %>%
  group_by(season) %>%
  summarise(
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score)
    ) %>% 
  mutate(
    mean_point_diff = home_points_diff / n
    )
  

#season, week wide
league_wide_win <- pbp %>%
  distinct(game_id, .keep_all = TRUE) %>%
  mutate(score_diff = home_score - away_score) %>%
  group_by(season, week) %>%
  summarise(
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score)
  ) %>% ungroup() %>%
  mutate(
    mean_point_diff = home_points_diff / n,
    week_n = row_number()
  ) 

league_wide_win %>%
  ggplot(aes(
    x = week_n, 
    y = mean_point_diff
    )) +
  geom_point(alpha = 0.6) +
  scale_x_continuous(
    breaks = seq(1, 358, 17), 
    labels = paste0("'", substr(c(2001:2022), 3, 4))
  ) +
  labs(
    x = "Season",
    y = "Mean Home Point Differential"
  )
 





mod <- lm(mean_point_diff ~ factor(season), data = league_wide_win)
summary(mod) 
#broom::tidy(mod)



league_wide_win %>%
  ggplot(aes(
    x = factor(season), 
    y = mean_point_diff, 
    group = 1)) +
  geom_line(aes(group = factor(week)), alpha = 1/2) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  labs(
    x = "Season",
    y = "Mean Home Point Differential"
  ) +
  stat_smooth(method = 'lm', col = '#984ea3', se = FALSE, size = 1) +
  scale_x_discrete(labels = paste0("'", substr(c(2001:2021), 3, 4))) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  hrbrthemes::theme_ft_rc()

league_wide_win <- league_wide_win %>%
  mutate(rel_hfa = resid(mod))

best_hfa <- league_wide_win %>% 
  group_by(team) %>%
  summarise(
    mean = mean(rel_ppg),
    sd = sd(rel_ppg),
    sem = sd(rel_ppg)/sqrt(n()),
    tpval = t.test(rel_ppg)$p.value
  ) %>%
  mutate(sig = tpval < .05)




# team

league_wide_win_team <- pbp %>%
  distinct(game_id, .keep_all = TRUE) %>%
  mutate(score_diff = home_score - away_score) %>%
  group_by(season, home_team) %>%
  summarise(
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score)
  ) %>% ungroup() %>%
  mutate(
    mean_point_diff = home_points_diff / n,
    week_n = row_number()
  ) 


league_wide_win_team %>%
  mutate(season_label = paste0(substr(season, 3, 4), "/", substr(season+1, 3, 4))) %>%
  ggplot(aes(
    x = factor(season_label), 
    y = mean_point_diff, group = 1)) +
  geom_line(aes(group = factor(home_team)), alpha = 1/2) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  scale_y_continuous(breaks = seq(-22, 24, 4)) +
  labs(
    x = "Season",
    y = "HFA"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  hrbrthemes::theme_ft_rc()


mod <- lm(mean_point_diff ~ factor(season), data = league_wide_win_team)
summary(mod)

# Step 3b: Plot this linear trend over time
league_wide_win_team %>%
  mutate(season_label = paste0(substr(season, 3, 4), "/", substr(season+1, 3, 4))) %>%
  ggplot(aes(
    x = factor(season_label), 
    y = mean_point_diff, group = 1)) +
  geom_line(aes(group = factor(home_team)), alpha = 1/2) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  labs(
    x = "Season",
    y = "HFA"
  ) +
  stat_smooth(method = 'lm', col = '#984ea3', se = FALSE, size = 1) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  hrbrthemes::theme_ft_rc()

# Step 4a: Remove this linear trend and focus on residuals
# This will examine teams' performance after controlling for this decrease in
# hfa in the last decade

league_wide_win_team <- league_wide_win_team %>%
  mutate(rel_hfa = resid(mod))



# Step 4b: Average the residuals for each team over the past decade. Then,
# perform a one-sample t-test (i.e., from zero) to see teams that were above
# or below the average power play goals/game despite the decrease in 
# power play goals in the last decade  

best_hfa <- league_wide_win_team %>% 
  group_by(home_team) %>%
  summarise(
    mean = mean(rel_hfa),
    sd = sd(rel_hfa),
    sem = sd(rel_hfa)/sqrt(n()),
    tpval = t.test(rel_hfa)$p.value
  ) %>%
  mutate(sig = tpval < .05)

print(best_hfa)




# Step 4c: Plot the average residuals, their standard error of the mean,
# and color based on if significantly different from 0 (i.e., average PPG/Game)

# -- To get y.axis bold conditional -- # 
best_hfa_sort <- arrange(best_hfa, mean)                       
axisFace <- ifelse(best_hfa_sort$sig == TRUE, 'bold', 'plain')
# -- To get y.axis bold conditional -- #

# -- To get shading conditional -- #
above <- best_hfa %>% 
  filter(mean > 0, sig == TRUE) %>% 
  arrange(mean)

average <- best_hfa %>% 
  filter(sig == FALSE)

below <- best_hfa %>% 
  filter(mean < 0, sig == TRUE) %>% 
  arrange(mean)
# -- To get shading conditional -- #


best_hfa %>%
  ggplot(aes(
    x = mean,
    y = fct_reorder(home_team, mean),
    colour = sig
  )) +
  geom_errorbarh(aes(
    xmin = mean - sem,
    xmax = mean + sem
  )) +
  geom_point(size = 2) +
  #scale_x_continuous(limits = (c(-.2, .2))) +
  scale_color_manual(
    values = c("grey", "black"),
    guide = guide_legend(
      reverse = TRUE,
      title = "Above/Below Average",
      title.position = "top"
    )
  ) +
  labs(
    x = "Mean HFA (Residuals)",
    y = "NFL Team",
    title = "NFL Home Field Advantage 2001-2021",
    caption = "Error bars are SEM."
  ) +
  hrbrthemes::theme_ft_rc() +
  theme(
    axis.text.y = element_text(face = axisFace),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.key.width = unit(1, "in"),
    legend.key.height = unit(.5, "in"),
    legend.title.align = .5,
    axis.title.x = element_text(vjust = -1)
  ) +
  annotate("rect",
           xmin = -Inf,
           xmax = Inf,
           ymax = above$home_team[nrow(above)],
           ymin = above$home_team[1],
           fill = "#486e48",
           colour = NA,
           alpha = 1 / 3
  ) +
  annotate("rect",
           xmin = -Inf,
           xmax = Inf,
           ymax = below$home_team[nrow(below)],
           ymin = below$home_team[1],
           fill = "#346991",
           colour = NA,
           alpha = 1 / 3
  ) +
  annotate("text", x = -.1, y = above$home_team[3], label = "Best Teams") +
  annotate("text", x = .1, y = below$home_team[3], label = "Worst Teams")





best_hfa %>%
  ggplot(aes(
    x = mean,
    y = fct_reorder(home_team, mean),
    colour = sig
  )) +
  geom_errorbarh(aes(
    xmin = mean - sem,
    xmax = mean + sem
  )) +
  geom_point(size = 2) +
  #scale_x_continuous(expand = expansion(mult = 0.2)) +
  #scale_x_continuous(limits = (c(-.2, .2))) +
  scale_color_manual(
    values = c("grey", "black"),
    guide = guide_legend(
      reverse = TRUE,
      title = "Above/Below Average",
      title.position = "top"
    )
  ) +
  labs(
    x = "Mean HFA (Residuals)",
    y = "NFL Team",
    title = "NFL Home Field Advantage 2001-2021",
    caption = "Error bars are SEM."
  ) +
  hrbrthemes::theme_ft_rc() +
  theme(
    axis.text.y = element_text(face = axisFace),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.key.width = unit(1, "in"),
    legend.key.height = unit(.5, "in"),
    legend.title.align = .5,
    axis.title.x = element_text(vjust = -1),
    plot.margin = unit(c(0.25, 0.25, 0, 0.35), "cm") #top, right, bottom, and left 
  ) 

