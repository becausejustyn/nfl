phi_color <- nflfastR::teams_colors_logos %>%
  filter(team_abbr == "PHI") %>%
  pull(team_color)

phi_color2 <- nflfastR::teams_colors_logos %>%
  filter(team_abbr == "PHI") %>%
  pull(team_color2)

cin_color <- nflfastR::teams_colors_logos %>%
  filter(team_abbr == "CIN") %>%
  pull(team_color)

cin_color2 <- nflfastR::teams_colors_logos %>%
  filter(team_abbr == "CIN") %>%
  pull(team_color2)

# Proportion of Pass/Run by Week

pbp %>%
  filter(play_type %in% c("run", "pass"), posteam == "PHI") %>%
  group_by(week, play_type) %>% #posteam
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


p1 <- pbp %>%
  filter(play_type == "pass", !is.na(air_yards)) %>%
  filter(posteam %in% c('PHI', 'CIN')) %>%
  ggplot(aes(
    x = air_yards, 
    fill = posteam)) +
  geom_histogram(binwidth = 2, alpha = 0.9) +
  scale_fill_manual(values = c(cin_color, phi_color)) +
  geom_hline(yintercept = 0, size = 1) +
  tomtom::theme_538() +
  guides(
    fill = guide_legend(
      label = TRUE, title = "", label.position = "left",
      direction = "vertical",
      label.theme = element_text(size = 20)
    )) +
  theme(legend.position = c(0.5, 0.9)) +
  scale_x_continuous(breaks = seq(-10, 60, 10)) +
  labs(
    x = "\nAir Yards",
    y = "Count",
    title = "Bengals threw more passes at all ranges",
    caption = "Data: @nflfastR | Plot: @becausejustyn"
  )

ggsave(p1, path = "plots", filename = "bengals_vs_phi_passing.png", dpi = 600)


p2 <- pbp %>%
  filter(play_type == "pass", !is.na(air_yards)) %>%
  filter(posteam %in% c('PHI', 'CIN')) %>%
  ggplot(aes(
    x = air_yards, 
    fill = posteam)) +
  geom_density(alpha = 0.8) +
  scale_fill_manual(values = c(cin_color, phi_color)) +
  tomtom::theme_538() +
  guides(
    fill = guide_legend(
      label = TRUE, title = "", label.position = "left",
      direction = "vertical",
      label.theme = element_text(size = 20)
    )) +
  theme(legend.position = c(0.5, 0.9)) +
  scale_x_continuous(breaks = seq(-10, 60, 10))

ggsave(p2, path = "plots", filename = "bengals_vs_phi_airyards.png", dpi = 600)

p3 <- pbp %>%
  filter(play_type %in% c("run", "pass"), posteam %in% c('PHI', 'CIN')) %>% 
  ggplot(aes(x = play_type, y = epa, color = posteam)) +
  geom_sina(alpha = 0.5) +
  scale_fill_manual(values = c(cin_color, phi_color), aesthetics = c("fill", "color")) +
  tomtom::theme_538() +
  theme(legend.position = "none") +
  facet_grid(~posteam)

ggsave(p3, path = "plots", filename = "bengals_vs_phi_playtype.png", dpi = 600)