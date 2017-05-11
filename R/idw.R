#' Inverse distance weighting
#'
#' @export
#'
idw <- function(x, dist, na.rm = TRUE) {
  s1 <-  sum(x / dist, na.rm = na.rm)
  s2 <-  sum(dist^-1, na.rm = na.rm)

  return(s1 / s2)
}
