#' @export
#'
complete_fill <- function(data, value) {
  x <- '[['(data, lazyeval::expr_text(value))

  all(!is.na(x))
}

#' @export
#'
key_miss <- function(data, key, value) {
  x <- '[['(data, lazyeval::expr_text(value))

  k <- '[['(dplyr::filter(data, is.na(x)), lazyeval::expr_text(key))

  unique(k)
}

#' @export
#'
prop_miss <- function(data, key, value) {
  x <- sum(is.na('[['(data, lazyeval::expr_text(value))))
  y <- nrow(data)

  x / y * 100
}

summa_key <- function(data, ...) {
  g <- dplyr::group_by_(
    data,
    lazyeval::lazy(key),
    lazyeval::lazy(lat),
    lazyeval::lazy(lon)
  )
  s <- dplyr::summarise(g)

  ungroup(s)
}

#' @export
#'
near_st <- function(data, key, lon, lat, value) {
  g <- dplyr::group_by_(
    data,
    lazyeval::lazy(key),
    lazyeval::lazy(lat),
    lazyeval::lazy(lon)
  )
  s <- dplyr::summarise(g)

  x <- dplyr::ungroup(s)

  y <- search_nearest_st(
    '[['(x, lazyeval::expr_text(key)),
    '[['(x, lazyeval::expr_text(lon)),
    '[['(x, lazyeval::expr_text(lat))
  )

  l_near <- dplyr::mutate(
    x,
    nearest = y
  )

  z <- '[['(l_near, lazyeval::expr_text(key)) == value

  '['(l_near, z, "nearest")[[1]]
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
