
library(tidyverse)
library(nflfastR)
library(scales)
library(becausejustynfun)

pbp <- load_pbp(seasons = 2021) %>%
  filter(season_type == 'REG')


gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", 
        #color = "transparent", 
        weight = px(2)
      ),
      locations = cells_body(
        columns = everything(),
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      #table.border.top.color = "transparent",
      #table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      #column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

rush_v_pass1 <- pbp %>% 
  filter(play_type %in% c("run", "pass"), penalty == 0) %>% 
  group_by(play_type, posteam) %>% 
  summarise(
    avg_epa = mean(epa, na.rm = TRUE),
    avg_yds = mean(yards_gained, na.rm = TRUE),
    .groups = "drop") 

rush_v_pass_sep1 <- rush_v_pass1 %>%
  pivot_wider(
    names_from = play_type,
    values_from = c(avg_yds, avg_epa)
  )

rush_v_pass_sep1_rank <- rush_v_pass_sep1 %>%
  mutate(avg_yds_pass_rank = rank(-avg_yds_pass), .after = avg_yds_pass) %>%
  mutate(avg_yds_run_rank = rank(-avg_yds_run), .after = avg_yds_run) %>%
  mutate(avg_epa_pass_rank = rank(-avg_epa_pass), .after = avg_epa_pass) %>%
  mutate(avg_epa_run_rank = rank(-avg_epa_run), .after = avg_epa_run) %>% 
  mutate(yard_rank_diff = avg_yds_run_rank - avg_yds_pass_rank,
         epa_rank_diff = avg_epa_run_rank - avg_epa_pass_rank
  ) %>%
  rename(team = posteam)


###################### Pass/Rush Yard Difference
############################################
############################################

rush_v_pass_sep1_rank %>%
  select(team:avg_yds_run_rank, yard_rank_diff) %>%
  mutate(yard_diff = avg_yds_pass - avg_yds_run) %>%
  mutate(across(c(avg_yds_pass, avg_yds_run, yard_diff), ~ round(., 2))) %>%
  gt() %>%
  tab_spanner(
    label = "PASSES",
    columns = c(avg_yds_pass, avg_yds_pass_rank)
  ) %>% 
  tab_spanner(
    label = "RUSHES",
    columns = c(avg_yds_run, avg_yds_run_rank)
  ) %>% 
  tab_spanner(
    label = "DIFFERENCE",
    columns = c(yard_diff, yard_rank_diff)
  ) %>%
  data_color(
    columns = c(yard_rank_diff),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  cols_label(
    yard_rank_diff = "YARD RANK",
    avg_yds_pass = "AVG YARDS",
    avg_yds_pass_rank = "RANK",
    avg_yds_run = "AVG YARDS",
    avg_yds_run_rank = "RANK",
    yard_diff = "YARDS"
  ) %>% 
  tab_source_note(
    source_note = md("Data: nflfastR<br>TABLE: @becausejustyn")
  ) %>% 
  tab_footnote(
    footnote = "Positive number indicates better at passing.",
    locations = cells_column_labels(
      columns = yard_rank_diff
    )) %>%
  gt_theme_538(table.width = px(550))


###################### Pass/Rush EPA Difference
############################################
############################################

rush_v_pass_sep1_rank %>%
  select(team, avg_epa_pass:avg_epa_run_rank, epa_rank_diff) %>%
  mutate(epa_diff = avg_epa_pass - avg_epa_run) %>%
  mutate(across(c(avg_epa_pass, avg_epa_run, epa_diff), ~ round(., 2))) %>%
  gt() %>%
  tab_spanner(
    label = "PASSES",
    columns = c(avg_epa_pass, avg_epa_pass_rank)
  ) %>% 
  tab_spanner(
    label = "RUSHES",
    columns = c(avg_epa_run, avg_epa_run_rank)
  ) %>% 
  tab_spanner(
    label = "DIFFERENCE",
    columns = c(epa_diff, epa_rank_diff)
  ) %>% 
  data_color(
    columns = c(epa_rank_diff),
    colors = scales::col_numeric(
      palette = c("white", "#3fc1c9"),
      domain = NULL
    )
  ) %>% 
  cols_label(
    epa_rank_diff = "EPA RANK DIFF",
    avg_epa_pass = "AVG EPA",
    avg_epa_pass_rank = "RANK",
    avg_epa_run = "AVG EPA",
    avg_epa_run_rank = "RANK",
    epa_diff = "EPA"
  ) %>% 
  tab_source_note(
    source_note = md("Data: nflfastR<br>TABLE: @becausejustyn")
  ) %>% 
  tab_footnote(
    footnote = "Positive number indicates better at passing.",
    locations = cells_column_labels(
      columns = epa_rank_diff
    )) %>%
  gt_theme_538(table.width = px(550))
