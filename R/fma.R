#' @export
#'
fma <- function(ur, ppt) {
  n <- length(ur)
  fma <- rep(NA_real_, n)

  for (i in seq_len(n)) {

    if (i == 1) {
      fma[i] <- 0
      next()
    }

    if (is.na(ur[i - 1])) {
      fma[i] <- 0
      next()
    }

    fma[i] <- case_when(
      ppt[i] < 2.5 ~ (100 / ur[i]) + fma[i - 1] * 1 ,
      ppt[i] >= 2.5 & ppt[i] < 5 ~ (100 / ur[i]) + fma[i - 1] * 0.7,
      ppt[i] >= 5   & ppt[i] < 10 ~ (100 / ur[i]) + fma[i - 1] * 0.4,
      ppt[i] >= 10  & ppt[i] < 13 ~ (100 / ur[i]) + fma[i - 1] * 0.2,
      ppt[i] >= 13 ~ 0
    )
  }

  fma
}

#' @export
#'
fma_classe <- function(fma) {
  case_when(
    fma <= 1 ~ 1,
    fma > 1 & fma <= 3 ~ 2,
    fma > 3 & fma <= 8 ~ 3,
    fma > 8 & fma <= 20 ~ 4,
    fma > 20 ~ 5
  )
}
