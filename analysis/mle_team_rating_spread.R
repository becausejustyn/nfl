

library(tidyverse)
library(alabama)
library(glue)

nfl_elo_latest <- read_csv("Downloads/nfl-elo/nfl_elo_latest.csv")


games <- nfl_elo_latest %>%  
    select(date, 
           neutral, 
           home = team1, 
           away = team2, 
           h_score = score1, 
           a_score = score2) %>% 
    mutate(h_mov = h_score - a_score)

current_season <- pull(nfl_elo_latest, season) %>% max()

#neutral games
glue("There was {games %>% nrow} games played in {current_season}. {pull(games, neutral) %>% sum()} of those games were played on a neutral field.")

#4 is less than 1% of games

#basic forcast function - Home Edge + Home Team Rating - Away Team Rating

home_forecast <- function(home_rating, visitor_rating, home_edge) {
  (home_edge + home_rating) - visitor_rating
}


#Create the ratings vector 

#vector with every nfl team
teams <- distinct(games, home) %>% arrange(home) %>% pull()

teams %>% length() == 32 # there's 32 NFL teams

#start by giving each team a rating of 0
# add an extra team for hfa
# add the team names to the ratings 

ratings <- rep(0, teams %>% length() +1)
names(ratings) <- c(teams, 'hfa') 


## The loss function

#`constrOptim.nl` is the function from the `alabama()` package that will perform the optimisation. The function’s argument `fn` is a 
# nonlinear objective function that is to be optimised. A scalar function that takes a real vector as argument and returns a scalar 
#that is the value of the function at that point.

#In other words, we need to create a function that sums the *squared errors* between the *forecast* and *actual game scores*. 
#`constrOptim.nl` *minimises* this number by trying out different team ratings. When a minimum is reached the algorithm will 
#stop and we can access the team ratings that best explain the results so far this season.

#The function is simple. Pass in the team ratings as the argument. The forecast for each game is calculated via the `home_forecast` 
#function. Each team’s rating is looked up by passing the home and away columns to ratings vector matching the ratings names to the 
#value in the columns. The home field edge is matched up with the 33rd element in the ratings vector. Next, a error column named resid 
#subtracts the forecast from the home team’s margin of victory, this value is squared. All of the squared errors are summed up in the `summarise` 
#function in the column sse. That scalar value is returned. This returned value is the number `constrOptim.nl` will attempt to minimize.


sqr_err <- function(ratings) {
  games %>%
    mutate(forecast = home_forecast(
      ratings[home],
      ratings[away],
      ratings[33]
    )) %>% # 33 is the hfa
    mutate(resid = (h_mov - forecast)^2) %>%
    summarise(sse = sum(resid)) %>%
    pull(sse)
}

#It is convenient to make our average team rating equal to 0. A team with a positive rating is better than average and a 
#team with a negative rating is worse than average.

set_team_avg_zero <- function(ratings) {
  h <- rep(NA, 1)
  h[1] <- mean(ratings[-33])
  h
}

set_team_avg_zero(ratings)

## Run the model

#From here it’s easy. We need to pass constrOptim.nl three arguments.

#1. `par` starting vector of parameter values. The `ratings` vector.
#2. `fn` function that is to be optimised. The `sqr_err` function.
#3. `heq` a vector function specifying equality constraints. The `set_team_avg_zero` function.

mod <- constrOptim.nl(
  par = ratings,
  fn = sqr_err,
  heq = set_team_avg_zero
)

#mod is a list of six items. The item we’re interested is `$par`
sse_ratings <- mod['par']

glue("These are our team ratings. According to the model, this season's home field advantage is {round(mod$par[['hfa']], 2)} points.")

# top 10 teams
tibble(
  teams = names(sse_ratings),
  rankings = sse_ratings) %>% 
  slice_max(rankings, n = 10)

glue("As of now the SSE is {round(mod$value,3)}")

## Using Ratings for Point Spreads

#Create a function that predicts the home team’s margin of victory by sticking the home_forecast function inside another function.

get_pointspred <- function(ratings, home, away) {
  home_forecast(
    ratings[home],
    ratings[away],
    ratings["hfa"]
  ) %>%
    round(2)
}

#Below is an example how you could use the ratings to predict point spreads. Obviously you want to predict games in the future but it works for an illustration.


nfl_elo_latest %>% 
  filter(date == '2022-02-13') %>% 
  select(date, home = team1, away = team2) %>% 
  mutate(spread = get_pointspred(mod$par, home, away)) %>% 
  knitr::kable()

