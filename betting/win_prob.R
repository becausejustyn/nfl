#I don't like these rates tbh

library(dplyr)

tibble(
  spread_favorite = seq(-17, 0, 0.5)
) %>%
  mutate(
    fav_win_prob = case_when(
      spread_favorite == 0 ~ 50.0,
      spread_favorite == -1 ~ 51.3,
      spread_favorite == -1.5 ~ 52.5,
      spread_favorite == -2 ~ 53.5,
      spread_favorite == -2.5 ~ 54.5,
      spread_favorite == -3 ~ 59.4,
      spread_favorite == -3.5 ~ 64.3,
      spread_favorite == -4 ~ 65.8,
      spread_favorite == -4.5 ~ 67.3,
      spread_favorite == -5 ~ 68.1,
      spread_favorite == -5.5 ~ 69,
      spread_favorite == -6 ~ 70.7,
      spread_favorite == -6.5 ~ 72.4,
      spread_favorite == -7 ~ 75.2,
      spread_favorite == -7.5 ~ 78.1,
      spread_favorite == -8 ~ 79.1,
      spread_favorite == -8.5 ~ 80.2,
      spread_favorite == -9 ~ 80.7,
      spread_favorite == -9.5 ~ 81.1,
      spread_favorite == -10 ~ 83.6,
      spread_favorite == -10.5 ~ 86,
      spread_favorite == -11 ~ 87.1,
      spread_favorite == -11.5 ~ 88.2,
      spread_favorite == -12 ~ 88.5,
      spread_favorite == -12.5 ~ 88.7,
      spread_favorite == -13 ~ 89.3,
      spread_favorite == -13.5 ~ 90,
      spread_favorite == -14 ~ 92.4,
      spread_favorite == -14.5 ~ 94.9,
      spread_favorite == -15 ~ 95.6,
      spread_favorite == -15.5 ~ 96.3,
      spread_favorite == -16 ~ 98.1,
      spread_favorite == -16.5 ~ 99.8,
      spread_favorite <= -17 ~ 100
    )
  )