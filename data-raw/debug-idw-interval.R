library(tidyverse)

base_clima <- forestr::weather %>%
  filter(id %in% c("A821", "A725", "A714")) %>%
  group_by(id, lon, lat, date) %>%
  summarise(prec = sum(prec)) %>%
  ungroup()

teste <- tribble(
  ~nome, ~lon, ~lat, ~d1, ~d2,
  "pnt1", -49.36, -23.39, "2017-07-02", "2017-07-09",
  "pnt1", -49.36, -23.39, "2017-07-02", "2017-07-09",
  "pnt1", -49.36, -23.39, "2017-07-02", "2017-07-09"
) %>%
  mutate_at(vars(d1, d2), as.Date)

ref_data <- base_clima
tgt_lon = -49.36
tgt_lat = -23.39
start = as.Date("2017-07-02")
end = as.Date("2017-07-09")
ref_key <- dplyr::quo(ref_key)
ref_value <- dplyr::quo(ref_value)

