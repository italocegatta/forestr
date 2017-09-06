library(dplyr)
library(lubridate)
library(inmetdown)

stations_id <- c("A725", "A708", "A714", "A716", "A726", "A711", "A740", "A713", "A842", "A821", "A763")
stations <- inmetdown::aws_stations() %>%
  filter(id %in% stations_id)

weather <- inmetdown::aws_import(
  stations_id,
  "01/07/2017",
  "10/07/2017"
  ) %>%
  left_join(stations, by = "id") %>%
  mutate(
    time = hour(date),
    date = as.Date(date)
  ) %>%
  select(id, city:alt, date, time, t_max:prec) %>%
  arrange(id, date, time)

devtools::use_data(weather, overwrite = TRUE)


