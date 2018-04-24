library(tidyverse)

base_clima <- read_rds("data-raw/base_dia.RDS") %>%
  filter(nome %in% c("A714", "A715", "A725")) %>%
  filter(data >= "2015-01-01", data <= "2017-12-01") %>%
  group_by(nome, lon, lat, data =  lubridate::floor_date(data, "month")) %>%
  summarise(ppt = sum(ppt, na.rm = TRUE)) %>%
  ungroup()


ref_data <- base_clima
tgt_lon = -49.36
tgt_lat = -23.39
start = as.Date("2016-07-02")
end = as.Date("2017-02-09")
ref_key <- dplyr::quo(data)
ref_value <- dplyr::quo(ppt)
ref_lon <- dplyr::quo(lon)
ref_lat <- dplyr::quo(lat)
fun <- sum
max_dist <- 200
source("R/haversine.R")


idw_interval_value(
  base_clima, data, ppt, lon, lat, -49.36, -23.39,
  as.Date("2016-07-02"), as.Date("2017-02-09"), mean)

teste <- tribble(
  ~nome, ~lon, ~lat, ~d1, ~d2,
  "pnt1", -49.36, -23.39, "2016-07-01", "2017-07-09",
  "pnt1", -49.36, -23.39, "2016-08-01", "2017-08-10",
  "pnt1", -49.36, -23.39, "2016-09-01", "2017-09-02"
) %>%
  mutate_at(vars(d1, d2), as.Date)

teste %>%
  rowwise() %>%
  mutate(ppt = idw_interval_value(base_clima, data, ppt, lon, lat, lon, lat,
                                  d1, d2, sum))
