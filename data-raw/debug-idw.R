library(forestr)
.data <- dplyr::mutate(forestr::weather, date_time = lubridate::ymd_h(paste(date, hour)))
key = dplyr::quo(date_time)
lon = dplyr::quo(lon)
lat = dplyr::quo(lat)
value = dplyr::quo(rh_min)
id = dplyr::quo(id)


data_i = df_i
data_nxt = df_nxt
