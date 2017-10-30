library(tidyverse)
library(forestr)

df <- data_frame(
  date = seq.Date(as.Date("2017-01-01"), as.Date("2017-12-01"), "month"),
  cad = 100,
  lat = -22.93,
  t_med = c(23.10, 23.50, 23.00, 21.10, 18.70, 17.40, 17.30, 18.90, 20.10, 21.20, 22.00, 22.50),
  prec =  c(240.20, 190.90, 147.30, 71.00, 65.10, 48.70, 36.80, 37.40, 65.60, 123.60, 137.50, 217.10)
  )

df_etp <- df %>%
  mutate(etp = etp(date, t_med, lat))

