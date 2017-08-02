library(dplyr)
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
  select(id, city:alt, date, t_max:prec) %>%
  arrange(id, date)

devtools::use_data(weather, overwrite = TRUE)
