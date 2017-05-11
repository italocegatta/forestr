#' Fill gab using IDW interpolation
#'
#' @export
#'
# raw <- inmetdown::aws_import(
#   c("A713", "A714", "A715", "A725", "A726", "A741", "A755", "A739"),
#     "25/04/2017",
#     "27/04/2017"
#   ) %>%
#   left_join(inmetdown::aws_stations(), by = "id") %>%
#   select(id, date, prec, lat, lon)
#
# aux <- c(0.6, 6.2, 10.0, 2.2, 1.4, 0.0, 0.2, .8, 2.0, 1.0, 0.2, 0.6, 14.8)
# raw[c(37:47, 107:108), ]$prec <- NA
#
# devtools::use_data(raw, overwrite = TRUE)
# #Base exemplo
# load("data/raw.rda")
#
# df = raw
# x = "prec"
# group = "date"
# lon = "lon"
# lat = "lat"
# fill_idw(raw, "prec", "date", "lon", "lat")
#
fill_idw <- function(df, x, group, lon, lat, dist = NULL) {

  # cria primary key
  df <- dplyr::mutate(df, .aux_id = 1:nrow(df))

  # separa dados faltantes
  a <- split(df, is.na(df[[x]]))

  # identifica datas faltantes
  k <- a[["TRUE"]][[group]]

  # separa dados para as id-datas faltantes
  b <- a[["FALSE"]][a[["FALSE"]][[group]] %in% k, ]

  # traz a lon e lat dos dados faltantes para a base de interpolacao
  b_join <- dplyr::left_join(b, a[["TRUE"]] , by = group)

  # calcula da distancia dentre os pontos disponiveis e as id-datas
  b_join$dis <- forestr::haversine(
    b_join$lon.x, b_join$lat.x,
    b_join$lon.y, b_join$lat.y
    )

  # calcula o idw para cada id-data faltante
  j <- dplyr::summarise_(
    dplyr::group_by_(b_join, ".aux_id.y", group),
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
  j_id <- dplyr::left_join(a[["TRUE"]], j, by = c(group, ".aux_id" = ".aux_id.y"))

  # subistitui NA por idw
  a[["TRUE"]]$prec <- j_id$.zz

  # junta id-data faltante com base interpolacao
  dplyr::bind_rows(a)[ , -ncol(a[["TRUE"]])]
}


