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

  k <- data[is.na(x), lazyeval::expr_text(key)][[1]]

  unique(k)
}

#' @export
#'
prop_miss <- function(data, key, value) {
  x <- sum(is.na(data[[lazyeval::expr_text(value)]]))
  y <- nrow(data)

  x / y * 100
}

#' @export
#'
list_key <- function(data, key, lon, lat) {
  g <- dplyr::group_by_(
    data,
    lazyeval::lazy(key),
    lazyeval::lazy(lon),
    lazyeval::lazy(lat)
  )
  s <- dplyr::summarise(g)

  ungroup(s)
}

#' @export
#'
near_st <- function(data, key, lon, lat, value) {
  x <- list_key(data, key, lon, lat)

  y <- search_nearest_st(
    x[[lazyeval::expr_text(key)]],
    x[[lazyeval::expr_text(lon)]],
    x[[lazyeval::expr_text(lat)]]
  )

  l_near <- dplyr::mutate(
    x,
    nearest = y
  )

  z <- l_near[[lazyeval::expr_text(key)]] == value

  l_near[z, "nearest"][[1]]
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
