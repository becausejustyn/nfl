### Running Back Receiving (Number of Targets vs Average Depth of Target)

source("https://raw.githubusercontent.com/leesharpe/nfldata/master/code/plays-functions.R")
library(ggthemes)
library(ggrepel)

# load nflscrapR pbp Dataset

pbp_all <- purrr::map_df(c(2021), function(x) {
  readRDS(
         glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
       )
   })

# since we want to work with the players GSIS-ID we load some rosterdata from ron
roster <- purrr::map_df(c(2021), function(x) {
     readRDS(
         glue::glue("~/Documents/nfl/data/roster/roster_{x}.rds")
       )
   })

# compute the dataframe with the charting data
chart <- pbp_all %>%
  filter(pass==1 & play==1 & !is.na(receiver_player_id) & !play_type=="note" & special==0 & !is.na(air_yards)) %>%
  group_by(receiver_player_id) %>%
  summarise(
    tar=n(),
    adot=mean(air_yards),
    name=first(receiver_player_name)
  ) %>%
  left_join(roster 
            %>% select(full_name, gsis_id, position, team), 
            by = c("receiver_player_id"="gsis_id")) %>%
  filter(tar>=30 & position=="RB") %>%
  arrange(desc(tar)) %>%
  left_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr"))

# create the plot
p1 <- chart %>%
  ggplot(aes(x = tar, y = adot)) +
  geom_hline(aes(yintercept = mean(adot)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = mean(tar)), color = "red", linetype = "dashed") +
  geom_point(colour = chart$team_color, fill = chart$team_color, alpha = 0.4, cex = 5) +
  geom_text_repel(aes(label = glue("{name} ({team})")), force = 1, point.padding = 0, segment.size = 0.1) +
  labs(x = "Targets",
       y = "Average Depth of Target - aDOT",
       caption = "Data: @nflscrapR",
       title = 'Receiving Performance by Running Backs in 2019',
       subtitle = "Number of Targets and Average Depth of Target for RBs with more than 30 Targets"
  )+
  theme_stata() +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 1))


ggsave(p1, path = "plots", filename = "rb_rec_adot_target.png", dpi = 600)