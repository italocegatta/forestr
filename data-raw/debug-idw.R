library(forestr)
.data <- dplyr::mutate(forestr::weather, date_time = lubridate::ymd_h(paste(date, hour)))
key = dplyr::quo(date_time)
lon = dplyr::quo(lon)
lat = dplyr::quo(lat)
value = dplyr::quo(t_min)