#' Inverse distance weighting
#'
#' @export
#'
idw <- function(x, dist, na.rm = TRUE) {
  s1 <-  sum(x / dist^2, na.rm = na.rm)
  s2 <-  sum(1 / dist^2, na.rm = na.rm)

  return(s1 / s2)
}
