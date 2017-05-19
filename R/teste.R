# library(dplyr)
# stations <- inmetdown::aws_stations() %>%
#   filter(state == "SP")
# ids <- stations$id
#
# sp_weather <- list()
# for( i in ids) {
#   sp_weather[[i]] <- inmetdown::aws_import(
#     i,
#     "01/05/2017",
#     "02/05/2017"
#   ) %>%
#   dplyr::left_join(stations, by = "id") %>%
#   dplyr::select(id, date, prec, lat, lon)
# }
#
# glimpse(sp_weather)
#
# sp_weather <- bind_rows(sp_weather)
#
# inmetdown::aws_import("A707")
#
# sp_weather %>%
#   group_by(id) %>%
#   summarise(n_falhas = is.na(prec))
#
#
#
# aux <- c(0.6, 6.2, 10.0, 2.2, 1.4, 0.0, 0.2, .8, 2.0, 1.0, 0.2, 0.6, 14.8)
# raw[c(37:47, 107:108), ]$prec <- NA
#
# devtools::use_data(raw, overwrite = TRUE)
