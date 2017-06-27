#' @export
#' # test if vector is fill
is_complete <- function(data, value) {
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

key_summarise <- function(data, ...) {
  group_by <- dplyr::quos(...)

  g <- dplyr::group_by(data, !!!group_by)
  s <- dplyr::summarise(g)
  dplyr::ungroup(s)
}

#' @export
#'
near_st <- function(data, key, lon, lat) {
  key <- dplyr::enquo(key)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)
  value <- dplyr::enquo(value)

  x <- key_summarise(data, !!key, !!lon, !!lat)

  dplyr::mutate(
    x,
    nearest = search_nearest(
      !!key,
      !!lon,
      !!lat
    )
  )
}


#' @export
#'
missing_cover <- function(data, data_key, key, value, key_time, nearest_key) {
  f1 <- lazyeval::interp(~is.na(x), x = lazyeval::lazy(value))
  date_i <- '[['(dplyr::filter_(data_key, f1), lazyeval::expr_text(key_time))

  f2 <- lazyeval::interp(
    ~k == n & kt %in% date_i,
    k = lazyeval::lazy(key),
    n = lazyeval::lazy(nearest_key),
    kt = lazyeval::lazy(key_time)
  )
  #f2
  value_i <- '[['(dplyr::filter_(data, f2), lazyeval::expr_text(value))
  #value_i

  sum(!is.na(value_i)) / length(value_i) * 100
}


#' @export
#'
fit_model <- function(data, key, value, station, nearest) {
  aux_st <- data[[lazyeval::expr_text(key)]] == station
  aux_near <- data[[lazyeval::expr_text(key)]] == nearest

  y <- data[aux_st, lazyeval::expr_text(value)][[1]]
  x <- data[aux_near, lazyeval::expr_text(value)][[1]]

  lm(y ~ x)
}

#' @export
#'
predict_value <- function(data, key, value, station, nearest) {
  aux_st <- data[[lazyeval::expr_text(key)]] == station
  aux_near <- data[[lazyeval::expr_text(key)]] == nearest

  y <- data[aux_st, lazyeval::expr_text(value)][[1]]
  x <- data[aux_near, lazyeval::expr_text(value)][[1]]

  k <- data[[lazyeval::expr_text(key)]]
  d <- data[[lazyeval::expr_text(date)]]
  v <- data[[lazyeval::expr_text(value)]]

  dd <- d[k == station & is.na(v)]

  vv <- v[k == nearest & d %in% dd]

  lm <- lm(y ~ x)
  as.numeric(predict(lm, data.frame(x = vv)))
}
