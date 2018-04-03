library(tidyverse)
library(forestr)
.data <- dplyr::mutate(forestr::weather, date_time = lubridate::ymd_h(paste(date, hour)))
.data <- read_csv2(
  "../teste_idw.csv",
  col_types =  cols(
    id = col_character(),
    lon = col_double(),
    lat = col_double(),
    date_time = col_date(format = ""),
    prec = col_double()
  )
)
key = dplyr::quo(date_time)
lon = dplyr::quo(lon)
lat = dplyr::quo(lat)
value = dplyr::quo(prec)
id = dplyr::quo(id)
radius = 200

data_i = df_i
data_nxt = df_nxt

.data %>%
  filter(date == "2017-07-05 ")

z %>%
  filter(is.na(prec))

.data %>%
  fill_idw(date_time, lon, lat, prec, radius = 200) %>%
  filter(is.na(prec))
