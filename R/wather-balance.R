rad <- function(x) {
  pi * x / 180
}

delta <- function(doy){
  23.45 * sin(rad(360 * (doy - 81) / 365))
}

hn <- function(lat, delta) {
  acos(-tan(rad(lat)) * tan(rad(delta))) * 180 / pi
}

n_days <- function(x) {

  last <- seq.Date(max(x), max(x) + 32, by = "month")[2]

  as.numeric(diff(c(x, last)))
}

n_hours <- function(date, lat) {

  delta <- delta(doy(date))
  hn <- hn(lat, delta)
  2 * hn / 15
}

#' @export
wb_i <- function(temperature){
  sum((0.2 * temperature)^1.514)
}

#' @export
wb_a <- function(i){
  #0.49239 + (1.7912 * (10^-2) * (sum(i))) - (7.7 * (10^-5) * ((sum(i))^2)) + (6.75 * (10^-7) * ((sum(i))^3))
  0.49 + 0.018 * i - 7.7 * (10^-5) * (i^2) + 6.75 * (10^-7) * (i^3)
}

#' @export
etp <- function(date, t_med, lat, i = NULL, a = NULL) {

  if (is.null(i)) {
    i <- wb_i(t_med)
  }

  if (is.null(a)) {
    a <- wb_a(i)
  }

  n_days <- n_days(date)
  n_hours <- n_hours(date, lat)

  ifelse(  # precisa vetorizar?
    t_med < 26.5,
    16 * ((10 * (t_med / i))^a) * (n_hours / 12) * (n_days / 30),
    (-415.85 + 32.24 * t_med - 0.43 * t_med^2) * (n_hours / 12) * (n_days/30)
  )
}

# .data = df_etp
# cad = "cad"
# prec = "prec"
# etp = "etp"

wb <- function(.data, cad, prec, etp) {

  prec <- dplyr::enquo(prec)
  etp <- dplyr::enquo(etp)
  cad <- dplyr::enquo(cad)

  p_etp <- pull(.data, !!prec) - pull(.data, !!etp)

}

# x <- p_etp
# i = 1
guess_start <- function(x) {

  #n = length(x)

  for (i in seq_along(x)) {
   guess[i] <- all(x[0:2 + i] > 0)
  }

  which(x, guess[1])

}

seq_cycle <- function(x, new) {

  stopifnot(new %in% x)

  if (x[1] == new) {
    return(x)
  }

  n <- length(x)
  i <- which(x == new)

  x[c(i:n, 1:(i-1))]
}
