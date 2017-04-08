#' Uniformity Index PV50
#'
#' @export
#'
pv50 <- function(x) {

  n_na <- x[is.na(x)]

  n50 <- length(x)/2

  sum <- sum(x, na.rm = TRUE)

  cum_sum <- c(n_na, cumsum(sort(x)))

  if (n50%%2L == 1L)
    sum50 <- mean(cum_sum[n50], na.rm = TRUE)
  else
    sum50 <- mean(cum_sum[n50 + 0L:1L], na.rm = TRUE)

  z <- sum50 / sum * 100

  return(z)
}
