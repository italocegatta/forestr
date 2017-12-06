library(tidyverse)
library(forestr)
.data <- dplyr::mutate(forestr::weather, date_time = lubridate::ymd_h(paste(date, hour)))
key = dplyr::quo(date_time)
lon = dplyr::quo(lon)
lat = dplyr::quo(lat)
value = dplyr::quo(prec)
id = dplyr::quo(id)
radius = 100

data_i = df_i
data_nxt = df_nxt

.data %>%
  filter(date == "2017-07-05 ")

z %>%
  filter(is.na(prec))

.data %>%
  fill_idw(date_time, lon, lat, prec, radius = 100) %>%
  fill_idw(date_time, lon, lat, prec, radius = 00) %>%
  filter(is.na(prec))
