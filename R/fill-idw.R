#' Fill gap with IDW interpolation
#'
#' @export
#'
fill_idw <- function(.data, key, lon, lat, value, radius = 100) {

  # problema quando a tabela tem o noma 'data' e da ambiguidade
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)


  #df <- dplyr::mutate(.data, zzz_id = seq_len(nrow(.data)))

  # primary key
  .data$zzz_id <- seq_len(nrow(.data))

  # split missing/fill values
  df_split <- split(.data, is.na(dplyr::pull(.data, !!value)))
  df_miss <- df_split[["TRUE"]]
  df_fill <- df_split[["FALSE"]]

  # trigger to complete data
  if (is.null(df_miss)) {
    return(.data)
  }

  # identify missing/fill keys
  k_miss <- dplyr::pull(df_miss, !!key)
  k_fill <- dplyr::pull(df_fill, !!key)


  # complete data filter from dates which has any missing value
  df_fill_key <- dplyr::filter(df_fill, k_fill %in% k_miss)


  # merge la, lon, value from missing data to df_fill_key
  df_fill_key_join <- dplyr::left_join(df_fill_key, df_miss , by = dplyr::quo_name(key))

  # calc distance between x and y points
  df_fill_key_join$dis <- haversine(
    df_fill_key_join[[paste0(dplyr::quo_name(lon), ".x")]],
    df_fill_key_join[[paste0(dplyr::quo_name(lat), ".x")]],
    df_fill_key_join[[paste0(dplyr::quo_name(lon), ".y")]],
    df_fill_key_join[[paste0(dplyr::quo_name(lat), ".y")]]
  )


  # filter minimum radius
  df_fill_key_join <- dplyr::filter(df_fill_key_join, dis <= radius)


  # falta retornar erro quando não achar nenhuma estacao
  #


  # calcula o idw para cada id-data faltante

  # aux names
  value_x <- paste0(dplyr::quo_name(value), ".x")
  value_xq <- rlang::parse_quosure(value_x)
  dis_q <- dplyr::quo(dis)


  # calc idw for each missing value.
  df_fill_key_join_g <- dplyr::group_by(df_fill_key_join, zzz_id.y, !!key)

  calc_idw <- dplyr::summarise(
    df_fill_key_join_g,
    .zz = idw(!!value_xq, !!dis_q)
  )


  # merge missing values to interpolate
  id_filled <- dplyr::left_join(df_miss, calc_idw, by = c(dplyr::quo_name(key), "zzz_id" = "zzz_id.y"))


  # replace NA to interpolate value
  df_miss_fill <- dplyr::mutate(df_miss, !!dplyr::quo_name(value) := id_filled$.zz)


  # bind all together
  z <- dplyr::arrange(dplyr::bind_rows(df_fill, df_miss_fill), zzz_id)[ , -ncol(df_fill)]

  z
}
