# stadium and surface variables

# stadium and surface type variables

var_pbp <- pbp %>%
  select(season, home_team, surface, roof) %>%
  distinct(home_team, season, .keep_all = TRUE) %>%
  dplyr::mutate(dplyr::across(
    .data$surface,
    ~ stringr::str_replace_all(., c(
      '"grass"' = 'grass',
      '"astroturf"' = 'astroturf',
      '"fieldturf"' = 'fieldturf',
      '"fieldturf "' = 'fieldturf',
      '"astroplay"' = 'astroplay',
      '"dessograss"' = 'dessograss',
      '"sportturf"' = 'sportturf',
      '"matrixturf"' = 'matrixturf',
      '"a_turf"' = 'a_turf',
      '"fieldturf"' = 'fieldturf'
    ))
  ),
  surface_model = as_factor(surface),
  turf = dplyr::if_else(.data$surface_model %in% c('a_turf', 'astroplay', 'astroturf', 'fieldturf', 'matrixturf', 'sportturf'), 1, 0),
  grass = dplyr::if_else(.data$surface_model %in% c('dessograss', 'grass'), 1, 0),  
  #NA, open and closed become retractable
  model_roof = dplyr::if_else(is.na(.data$roof) | .data$roof == 'open' | .data$roof == 'closed', as.character('retractable'), as.character(.data$roof)),
  model_roof = as.factor(.data$model_roof),
  retractable = dplyr::if_else(.data$model_roof == 'retractable', 1, 0),
  dome = dplyr::if_else(.data$model_roof == 'dome', 1, 0),
  outdoors = dplyr::if_else(.data$model_roof == 'outdoors', 1, 0)
  ) %>%
  select(-c(roof, model_roof, surface, surface_model))