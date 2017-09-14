#' Summarise by minimum observation
#'
#' @export
#'
summ_var <- function(vec, fun = mean, n_min = 15, ...) {
  n_fill <- sum(!is.na(vec))

  if (n_fill >= n_min) {
    return(fun(vec, na.rm = TRUE, ...))
  }

  NA
}

#' Summarise value by groups and minimum observation
#'
#' @export
#'
summ_var_part <- function(key, vec, fun = mean, groups = 3, n_min = 6, ...) {
  vec_cut <- split(vec, cut(key, breaks = groups))

  vec_fun <- purrr::map_dbl(vec_cut, ~summ_var(.x, fun, n_min))

  if (!all(!is.na(vec_fun))) {
    return(NA)
  }

  fun(vec_fun, ...)
}