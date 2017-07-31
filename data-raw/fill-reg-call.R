fill_reg <- function(data, id, key, value, lon, lat) {
  id <- dplyr::enquo(id)
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)

  if (is_complete(data, !!value)) return(df)

  list_id_miss <- id_miss(data, !!id, !!value)
  print(list_id_miss)
  #i = 1
  for (i in seq_along(list_id_miss)) {
    df_i <- dplyr::filter(data, (!!id) == list_id_miss[i])

    nearest_i <- near_st(data, !!id, !!lon, !!lat, list_id_miss[i])
    df_nxt <- dplyr::filter(df, (!!id) == nearest_i)

    print(list_id_miss[i])
    print(nearest_i)

    if (prop_miss(df_i, !!id, !!value) > 70) {
      stop("tentar IDW")
    }

    # print(df_i$t_min)
    # print(" ")
    # print(df_nxt$t_min)

    if (coverage(df_i, df_nxt, !!id, !!key, !!value) != 100) {
      message(paste(list_id_miss[i], "and", nearest_i, "don't have coverage"))
    }

    lm_i <- fit_model(df_i, df_nxt, !!value)

    if (summary(lm_i)$r.squared < 0.8) {
      message(paste(list_id_miss[i], "and", nearest_i, "don't have nice fit"))
    }
  }
}

library(forestr)
library(dplyr)

df = dplyr::select(sp_weather, estacao = id, data = date, t_min, lon, lat)

fill_reg(df, estacao, data, t_min, lon, lat)

