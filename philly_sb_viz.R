library(tidyverse)

post_pbp_2017 <- read_csv(
  "https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/play_by_play_data/post_season/post_pbp_2017.csv")

super_bowl_pbp <- post_pbp_2017 %%
  filter(game_id == 2018020400) %%
  select(
    posteam, defteam, drive, qtr, down, ydstogo, yardline_100,
    half_seconds_remaining, score_differential, desc, play_type, 
    yards_gained, sp, ep, wp, epa, wpa
  )



ne_color <- nflfastR::teams_colors_logos %>% filter(team_abbr == 'NE')
phi_color <- nflfastR::teams_colors_logos %>% filter(team_abbr == 'PHI')
 
super_bowl_pbp %>%
     filter(play_type %in% c("pass", "run")) %>%
     group_by(posteam, down, play_type) %>%
     count() %>%
     filter(!is.na(down)) %>%
     group_by(posteam, down) %>%
     mutate(n_plays = sum(n),
                       prop_plays = n / n_plays) %>%
     ggplot(aes(x = play_type, y = prop_plays, fill = posteam)) +
     geom_bar(stat = "identity", position = "dodge") +
     facet_wrap(~down, ncol = 4) +
     scale_fill_manual(values = c(ne_color$team_color, phi_color$team_color)) +
     labs(x = "Play type", 
          y = "Proportion of plays",
                   fill = "Team",
                   title = "Comparison of Patriots and Eagles play-calling by down") +
     theme_bw()


super_bowl_pbp %>%
  filter(play_type %in% c("pass", "run")) %>%
  group_by(posteam, down, play_type) %>%
  filter(!is.na(down)) %>%
  # Now use the summarise function, generate the average yards gained:
  summarize(yards_per_play = mean(yards_gained)) %>%
  ggplot(aes(x = play_type, y = yards_per_play, fill = posteam)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~down, ncol = 4) +
  scale_fill_manual(values = c(ne_color$team_color, phi_color$team_color)) +
  labs(
    x = "Play type",
    y = "Yards per play",
    fill = "Team",
    title = "Comparison of NE and PHI yards gained per play by type and down"
  ) +
  theme_bw()


post_pbp_2017 %>%
     # Only grab the Super Bowl
     filter(game_id == 2018020400) %>%
     filter(!is.na(home_wp),
                       !is.na(away_wp),
                       timeout == 0) %>%
     select(game_seconds_remaining,
                       home_wp,
                       away_wp) %>%
     # Rather than having separate columns for each team's win probability,
     # we can gather them into one column:
     gather(team, wpa, -game_seconds_remaining) %>%
     ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
     geom_line(size = 2) +
     geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
     scale_color_manual(labels = c("PHI", "NE"),
                                               values = c(phi_color$team_color, ne_color$team_color2),
                                               guide = "none") +
     scale_x_reverse(breaks = seq(0, 3600, 300)) + 
     annotate("text", x = 3000, y = .80, label = "PHI", color = phi_color$team_color, size = 8) + 
     annotate("text", x = 3000, y = .20, label = "NE", color = ne_color$team_color2, size = 8) +
  geom_vline(xintercept = seq(0, 2700, 900), linetype = "dashed", color = "black") +
     labs(
         x = "Time Remaining (seconds)",
         y = "Win Probability",
         title = "Super Bowl LII Win Probability Chart",
         subtitle = "New England Patriots vs Philadelphia Eagles",
         caption = "Data from nflscrapR") + 
     theme_bw()