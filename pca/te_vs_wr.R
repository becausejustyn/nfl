library(tidyverse)
library(tidymodels)

rec_stats1 <- rec_stats %>% na.exclude()

#map_dbl(rec_stats1, ~sum(is.na(.)))

pca_rec <- recipe(~., data = rec_stats1) %>%
  update_role(
    player_display_name, player_position, team_abbr, player_gsis_id, position, 
    status, full_name, pfr_id, headshot_url, player_label, receiver, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

pca_prep <- prep(pca_rec)

tidied_pca <- tidy(pca_prep, 2)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:5)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

library(tidytext)

tidied_pca %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )

juice(pca_prep) %>%
  ggplot(aes(PC1, PC2, label = player_label)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "Lato") +
  labs(color = NULL) +
  theme(legend.position = "none")

library(embed)

umap_rec <- recipe(~., data = rec_stats1) %>%
  update_role(
    player_display_name, player_position, team_abbr, player_gsis_id, position, 
    status, full_name, pfr_id, headshot_url, player_label, receiver, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())

umap_prep <- prep(umap_rec)

juice(umap_prep) %>%
  ggplot(aes(UMAP1, UMAP2, label = player_label)) +
  geom_point(alpha = 0.7, size = 2) +
  #geom_point(aes(color = posteam), alpha = 0.7, size = 2) +
  geom_text(check_overlap = TRUE, hjust = "inward", family = "Lato") +
  theme(legend.position = "none") +
  labs(color = NULL)




tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = 'Total Explained Variance = 100'

library(plotly)

juice(pca_prep) %>%
  plot_ly(
    x = ~PC1, y = ~PC2, z = ~PC3, text = .$player_label, color = ~position, colors = c('#636EFA','#EF553B','#00CC96')
  ) %>%
  add_markers(size = 12)

