
library(nflfastR)

#Get Season 2020 Schedule and results
nfl_games <- fast_scraper_schedules(2021) %>% 
  #Weeks Beyond Week 17 Are the Playoffs
  filter(game_type == 'REG')

#Method 1: Pythagorean expectation

p_wins <- nfl_games %>% 
  pivot_longer(
    cols = c(contains('team')),
    names_to = "category",
    values_to = 'team'
  ) %>% 
  mutate(
    points_for = (category == 'home_team') * home_score + (category == 'away_team') * away_score,
    points_against = (category == 'away_team') * home_score + (category == 'home_team') * away_score
  ) %>% 
  group_by(team) %>%
  summarize(
    pf = sum(points_for, na.rm = T),
    pa = sum(points_against, na.rm = T),
    actual_wins = sum(points_for > points_against, na.rm = T),
    .groups = 'drop'
  ) %>% 
  mutate(p_expectation = pf^2.37/(pf^2.37+pa^2.37)*17)


#Who outkicked their coverage?

library(ggrepel)

p_wins %>% 
  mutate(diff_from_exp = actual_wins - p_expectation) %>% 
  ggplot(aes(x = actual_wins, y = p_expectation, fill = diff_from_exp)) + 
  geom_label_repel(aes(label = team)) + 
  geom_abline(lty = 2) + 
  scale_x_continuous(breaks = seq(3, 14, 1)) +
  scale_y_continuous(breaks = seq(3, 14, 1)) +
  annotate("label", x = 1, y = 10, hjust = 'left', label = "Underachievers") +
  annotate("label", x = 10, y = 5, hjust = 'left', label = "Overachievers") +
  labs(x = "Actual Wins", y = "Expected Wins", 
       title = "What NFL Teams Over/Under Performed?", 
       caption = "Expected Wins Based on Pythagorian Expectation") + 
  scale_fill_gradient2(guide = "none") + 
  cowplot::theme_cowplot()


#Method #2: Simulation with Bradley-Terry Models

#BTm(cbind(win1, win2), team1, team2, ~ team, id = "team", data = sports.data)

#Get List of All Teams
all_teams <- sort(unique(nfl_games$home_team))

nfl_shaped <- nfl_games %>%
  mutate(
    home_team = factor(home_team, levels = all_teams),
    away_team = factor(away_team, levels = all_teams),
    home_wins = if_else(home_score > away_score, 1, 0),
    away_wins = if_else(home_score < away_score, 1, 0) 
  ) %>% 
  group_by(home_team, away_team) %>% 
  summarize(
    home_wins = sum(home_wins),
    away_wins = sum(away_wins),
            .groups= 'drop') 

#Fitting the Bradley-Terry Model

library(BradleyTerry2)

base_model <- BTm(cbind(home_wins, away_wins), home_team, away_team,
                  data = nfl_shaped, id = "team")

#Extracting the Team Abilities

base_abilities <- qvcalc(BTabilities(base_model)) %>% 
  .[["qvframe"]] %>% 
  as_tibble(rownames = 'team') %>% 
  janitor::clean_names()

#Simulating Playoff Matchups

playoff_teams = c('ARI', 'BUF', 'CIN', 'DAL', 'GB', 'NE', 'KC', 'LA', 'PHI',
                  'PIT', 'SF', 'TB', 'TEN', 'LV')

comparisons <- base_abilities %>% 
  filter(team %in% playoff_teams)

#Generate All Potential Combination of Playoff Teams
comparisons <- comparisons %>% 
  rename_with(~paste0("t1_", .x)) %>% 
  crossing(comparisons %>% rename_with(~paste0("t2_", .x)))  %>% 
  filter(t1_team != t2_team)

#Run 1000 Simulations per comparison
set.seed(20210107)

#Draw from Ability Distribution
simulations <- comparisons %>% 
  crossing(simulation = 1:1000) %>% 
  mutate(
    t1_val = rnorm(n(), t1_estimate, t1_quasi_se),
    t2_val = rnorm(n(), t2_estimate, t2_quasi_se),
    t1_win = t1_val > t2_val,
    t2_win = t2_val > t1_val
  )

#Roll up the 1000 Results
sim_summary <- simulations %>% 
  group_by(t1_team, t2_team, t1_estimate, t2_estimate) %>% 
  summarize(t1_wins_pct = mean(t1_win), #Long-Term Average Winning % for Team 1
            t2_wins_pct = mean(t2_win), #Long-Term Average Winning % for Team 2
            .groups = 'drop') %>% 
  mutate(
    #Create a label for the winner
    winner = if_else(t1_wins_pct > t2_wins_pct, t1_team, t2_team)
  )

