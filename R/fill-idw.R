#' Fill gap with IDW interpolation
#'
#' @export
#'
fill_idw <- function(data, key, lon, lat, value, radius = 100) {

  # testar quando não tiver obs vazias
  #
  # primary key
  data <- dplyr::mutate(data, .aux_id = 1:nrow(data))

  # splt missing values
  a <- split(data, is.na(data[[value]]))

  # identifica datas faltantes
  k <- a[["TRUE"]][[key]]

  # separa dados para as id-datas faltantes
  b <- a[["FALSE"]][a[["FALSE"]][[key]] %in% k, ]

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
  b_join <- dplyr::filter(b_join, dis < radius)

  # calcula o idw para cada id-data faltante
  j <- dplyr::summarise_(
    dplyr::group_by_(b_join, ".aux_id.y", key),
    .dots =  list(
      .zz = lazyeval::interp(
        ~forestr::idw(v, d),
        .values = list(
          v = as.name(paste0(value, ".x")),
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


#' Inverse distance weighting
#'
#' @export
#'
idw <- function(x, dist, na.rm = TRUE) {
  s1 <-  sum(x / dist^2, na.rm = na.rm)
  s2 <-  sum(1 / dist^2, na.rm = na.rm)

  s1 / s2
}
