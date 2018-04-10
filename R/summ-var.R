#' Summarise by minimum observation
#'
#' @export
#'
summarise_var <- function(vec, fun = mean, n_min = 18, ...) {
  n_fill <- sum(!is.na(vec))

  if (n_fill >= n_min) {
    return(fun(vec, na.rm = TRUE, ...))
  }

  NA_real_
}

#' Summarise value by groups and minimum observation
#'
#' @export
#'
summarise_var_part <- function(key, vec, fun = mean, groups = 3, n_min = 6, ...) {
  nb <- as.integer(groups + 1)
  dx <- diff(rx <- range(key, na.rm = TRUE))
  if (dx == 0) {
    dx <- abs(rx[1L])
    breaks <- seq.int(rx[1L] - dx/1000, rx[2L] + dx/1000, length.out = nb)
  } else {
    breaks <- seq.int(rx[1L], rx[2L], length.out = nb)
  }

  vec_cut <- split(vec, .bincode(key, breaks, TRUE, TRUE))
  #vec_na <- purrr::map_dbl(vec_cut, ~sum(!is.na(.x)))
  vec_na <- sapply(vec_cut, function(x) sum(!is.na(x)), simplify = TRUE)

  if (any(vec_na < n_min)) {
    return(NA_real_)
  } else {
    fun(vec, ...)
  }
}
