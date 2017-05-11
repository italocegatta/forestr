#' Haversine distance
#'
#' Haversine distance between two points given their longitude and latitude
#'
#' @export
#'
haversine <- function(lon1, lat1, lon2, lat2) {
  rad <- pi/180
  R <- 6378.1

  dlon <- (lon2 - lon1) * rad
  dlat <- (lat2 - lat1) * rad

  a <- (sin(dlat/2))^2 +
       cos(lat1 * rad) *
       cos(lat2 * rad) *
       (sin(dlon/2))^2

  c <- 2 * atan2(sqrt(a), sqrt(1 - a))

  d <- R * c

  return(d)
}
