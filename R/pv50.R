#' Uniformity Index PV50
#'
#' @export
#'
pv50 <- function(x) {

  n_na <- x[is.na(x)]

  n50 <- length(x)/2

  # para 5 arv, pega a 3 posicao
  # para 6 arv, pega a 3 posicao
  if (n50%%2L != 1L) {
    n50 <- ceiling(n50)
  }

  sum <- sum(x, na.rm = TRUE)

  cum_sum <- c(n_na, cumsum(sort(x)))

  cum_sum[n50] / sum * 100
}
