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
# sp_weather <- bind_rows(sp_weather)
#
#
# sp_weather %>%
#   group_by(id) %>%
#   summarise(n_falhas = sum(is.na(prec))) %>%
#   arrange(-n_falhas)


#devtools::use_data(sp_weather)

