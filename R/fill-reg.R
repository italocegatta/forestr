library(tidyverse)
library(forestr)

df = sp_weather
x = "t_min"
id = "id"
lon = "lon"
lat = "lat"


fill_id <- function(df, x, id, lon, lat) {

  # testar quando não tiver obs vazias
  #
  # primary key
  backup <- dplyr::mutate(df, .aux_id = 1:nrow(df))

  df_base <- backup[ , c("id", "t_min", "lon", "lat") ]

  # splt missing values
  a <- split(df_base, is.na(df_base[[x]]))

  # identifica os ids faltantes
  k <- unique(a[["TRUE"]][[id]])

  # estacoes candidatas
  candi <- df %>%
    group_by(id, lat, lon) %>%
    summarise() %>%
    ungroup() %>%
    mutate(candidate = search_nearest_st(id, lon, lat, 1)) %>%
    select(id, candidate) %>%
    filter(id %in% k)

  miss_cand <- dplyr::filter(a[["FALSE"]], id %in% c("A718", "A707")) %>%
    mutate(.aux_id = 1:nrow(.)) %>%
    tidyr::spread(id, t_min)

  ggplot(miss_cand, aes(A707, A718)) +
    geom_point()

  #purrr::map(a, ~dplyr::filter(.x, id %in% c("A718", "A707")))

  # separa dados para as id-datas faltantes
  b <- a[["FALSE"]][a[["FALSE"]][[key]] %in% k, ]



with(aux, search_nearest_st(id, lon, lat, 1))




  # traz a lon e lat dos dados faltantes para a base de interpolacao
  b_join <- dplyr::left_join(b, a[["TRUE"]] , by = key)

  # calcula da distancia dentre os pontos disponiveis e as id-datas
  b_join$dis <- forestr::haversine(
    b_join$lon.x, b_join$lat.x,
    b_join$lon.y, b_join$lat.y
  )

  # limita o raio de busca para interpolacao
  #
  # falta retornar erro quando não achar nenhuma estacao
  #b_join <- dplyr::filter(b_join, dis < radius)

  # calcula o idw para cada id-data faltante
  j <- dplyr::summarise_(
    dplyr::group_by_(b_join, ".aux_id.y", key),
    .dots =  list(
      .zz = lazyeval::interp(
        ~forestr::idw(v, d),
        .values = list(
          v = as.name(paste0(x, ".x")),
          d = quote(dis)
        )
      )
    )
  )

  # junta id-data com idw
  j_id <- dplyr::left_join(a[["TRUE"]], j, by = c(key, ".aux_id" = ".aux_id.y"))

  # subistitui NA por idw
  a[["TRUE"]]$prec <- j_id$.zz

  # junta id-data faltante com base interpolacao
  z <- dplyr::arrange(dplyr::bind_rows(a), .aux_id)[ , -ncol(a[["TRUE"]])]

  return(z)
}
