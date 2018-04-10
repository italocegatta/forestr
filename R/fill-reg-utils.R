
#
is_complete <- function(.data, value) {
  # test if vector is fill
  value <- dplyr::enquo(value)

  vec <- dplyr::pull(.data, !!value)
  all(!is.na(vec))
}

#
id_miss <- function(.data, id, value) {
  id <- dplyr::enquo(id)
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(.data, !!value))

  k <- dplyr::pull(.data[vec, ], !!id)

  unique(k)
}

# prop_miss(df_i, !!value) > 50
prop_miss <- function(.data, value) {
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(.data, !!value))
  sum(vec) / nrow(.data) * 100
}

#
summarise_group <- function(.data, ...) {
  group_by <- dplyr::quos(...)

  g <- dplyr::group_by(.data, !!!group_by)
  s <- dplyr::summarise(g)
  dplyr::ungroup(s)
}

#
nxt_id <- function(.data, id, lon, lat, target) {

  id <- dplyr::enquo(id)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)

  x <- unique(dplyr::select(.data, !!id, !!lon, !!lat))

  y <- search_nearest(x, !!id, !!lon, !!lat)

  dplyr::filter(y, (!!id) == target)[[2]]
}

#
coverage <- function(data_i, data_nxt, key, value) {
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)

  # join paired values
  join <- dplyr::left_join(
    dplyr::select(data_i, !!key, !!value),
    dplyr::select(data_nxt, !!key, !!value),
    by = dplyr::quo_name(key)
  )

  # just non missing values
  join_full <- na.omit(join)

  if (nrow(join_full) == 0) {
    return(0)
  }

  # coverage in percert
  nrow(join_full) / nrow(join) * 100
}

#
fit_model <- function(data_i, data_nxt, key, value) {
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)

  # join paired values
  join <- dplyr::left_join(
    dplyr::select(data_i, !!key, !!value),
    dplyr::select(data_nxt, !!key, !!value),
    by = dplyr::quo_name(key)
  )

  # just non missing values
  join_full <- na.omit(join)

  lm(join_full[[3]] ~ join_full[[2]])
}

#
predict_value <- function(model, data_i, data_nxt, value) {
  value <- dplyr::enquo(value)

  x <- dplyr::pull(data_nxt, !!value)
  y <- dplyr::pull(data_i, !!value)

  vec <- is.na(y)

  pred <- as.numeric(predict(model, data.frame(x = x[vec])))

  y[is.na(y)] <- round(pred, 1)

  y
}
