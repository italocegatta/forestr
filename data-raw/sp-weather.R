library(dplyr)
stations <- inmetdown::aws_stations() %>%
  filter(state == "SP")
ids <- stations$id

sp_weather <- list()
for( i in ids) {
  sp_weather[[i]] <- inmetdown::aws_import(
    i,
    "01/05/2017",
    "21/05/2017"
  ) %>%
  dplyr::left_join(stations, by = "id")
}

sp_weather <- bind_rows(sp_weather) %>%
  select(-url)

sp_weather <-   dplyr::filter(
  sp_weather,
  ! id %in% c("A763","A705", "A718", "A768")
)

devtools::use_data(sp_weather, overwrite = TRUE)


