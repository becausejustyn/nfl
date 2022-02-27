
library(tidyverse)
library(nflfastR)


pbp <- nflfastR::load_pbp(2021) %>% 
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))

offense <- pbp %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))

defense <- pbp %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))

combined <- offense %>%
  dplyr::inner_join(defense, by = "team")

# this is the same. you can do it in one chunk if you want
combined1 <- pbp %>%
  dplyr::group_by(team = posteam) %>%
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE)) %>%
  left_join(
    pbp %>%
      dplyr::group_by(team = defteam) %>%
      dplyr::summarise(def_epa = mean(epa, na.rm = TRUE)),
    by = c("team")
  )


qbs <- pbp %>%
  dplyr::filter(pass == 1 | rush == 1) %>%
  dplyr::filter(down %in% 1:4) %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(
    name = dplyr::first(name),
    team = dplyr::last(posteam),
    plays = dplyr::n(),
    qb_epa = mean(qb_epa, na.ram = TRUE)
  ) %>%
  dplyr::filter(plays > 200) %>%
  dplyr::slice_max(qb_epa, n = 10)



## Logos in Scatter Plots

combined %>%
  ggplot2::ggplot(aes(
    x = off_epa, 
    y = def_epa
    )) +
  ggplot2::geom_abline(slope = -1.5, intercept = seq(0.4, -0.3, -0.1), alpha = .2) +
  nflplotR::geom_mean_lines(aes(v_var = off_epa , h_var = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: nflfastR",
    title = "2021 NFL Offensive and Defensive EPA per Play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot"
  ) +
  ggplot2::scale_y_reverse()


## Logos as Axis Labels

### Off

ggplot2::ggplot(offense, aes(
  x = fct_reorder(team, off_epa), #team 
  y = off_epa
  )) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2021 NFL Offensive EPA per Play",
    y = "Offense EPA/play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.x = nflplotR::element_nfl_logo()
  )

### Def

defense %>%
  ggplot2::ggplot(aes(
  y = fct_reorder(team, def_epa, .desc = TRUE), 
  x = def_epa
  )) +
  ggplot2::geom_col(aes(color = team, fill = team), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.4) +
  ggplot2::labs(
    title = "2021 NFL Defensive EPA per Play",
    x = "Defense EPA/play"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    # it's obvious what the y-axis is so we remove the title
    axis.title.y = ggplot2::element_blank(),
    # this line triggers the replacement of team abbreviations with logos
    axis.text.y = nflplotR::element_nfl_logo(color = "b/w")
  )