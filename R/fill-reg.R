

#' @export
#'
is_complete <- function(data, value) {
  # test if vector is fill
  value <- dplyr::enquo(value)

  vec <- dplyr::pull(data, !! value)
  all(!is.na(vec))
}

#' @export
#'
id_miss <- function(data, id, value) {
  id <- dplyr::enquo(id)
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(data, !! value))

  k <- dplyr::pull(data[vec, ], !! id)

  unique(k)
}

#' @export
#'
prop_miss <- function(data, id, value) {
  id <- dplyr::enquo(id)
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(data, !! value))
  sum(vec) / nrow(data) * 100
}

summarise_group <- function(data, ...) {
  group_by <- dplyr::quos(...)

  g <- dplyr::group_by(data, !!!group_by)
  s <- dplyr::summarise(g)
  dplyr::ungroup(s)
}

#' @export
#'
near_st <- function(data, id, lon, lat, target) {

  id <- dplyr::enquo(id)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)

  x <- unique(dplyr::select(data, !!id, !!lon, !!lat))

  y <- search_nearest(x, !!id, !!lon, !!lat)

  dplyr::filter(y, (!!id) == target)[[2]]
}


#' @export
#'
coverage <- function(data_i, data_nxt, id, key, value) {
  id <- dplyr::enquo(id)
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)


  # missing values
  df_x <- dplyr::filter(
    dplyr::select(data_i, !!key, !!value), is.na(!!value)
  )

  # vector from next id
  df_y <- dplyr::select(data_nxt, !!key, !!value)

  # key values match
  join <- dplyr::left_join(df_x, df_y, by = dplyr::quo_name(key))

  # fill vector
  fill_vec <- !is.na(dplyr::pull(join, 3))
  if (all(!fill_vec)) {
    return(0)
  }

  # coverage in percert
  sum(fill_vec) / nrow(join) * 100
}


#' @export
#'
fit_model <- function(data_i, data_nxt, value) {
  value <- dplyr::enquo(value)

  x <- dplyr::pull(data_nxt, !!value)
  y <- dplyr::pull(data_i, !!value)

  lm(y ~ x)
}

#' @export
#'
predict_value <- function(data_i, data_nxt, value, model) {
  value <- dplyr::enquo(value)

  x <- dplyr::pull(data_nxt, !! value)
  y <- dplyr::pull(data_i, !! value)

  vec <- is.na(y)

  pred <- as.numeric(predict(model, data.frame(x = x[vec])))

  y[is.na(y)] <- round(pred, 1)

  y
}
