#' Calculate Potential Evapotranspiration using Thornthwaite method
#'
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
    round(16 * ((10 * (t_med / i))^a) * (n_hours / 12) * (n_days / 30)),
    round((-415.85 + 32.24 * t_med - 0.43 * t_med^2) * (n_hours / 12) * (n_days/30), 2)
  )
}


wb_i <- function(temperature){
  round(sum((0.2 * temperature)^1.514), 3)
}

wb_a <- function(i){
  #0.49239 + (1.7912 * (10^-2) * (sum(i))) - (7.7 * (10^-5) * ((sum(i))^2)) + (6.75 * (10^-7) * ((sum(i))^3))
  round(0.49 + 0.018 * i - 7.7 * (10^-5) * (i^2) + 6.75 * (10^-7) * (i^3), 3)
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

rad <- function(x) {
  pi * x / 180
}

delta <- function(doy){
  23.45 * sin(rad(360 * (doy - 81) / 365))
}

hn <- function(lat, delta) {
  acos(-tan(rad(lat)) * tan(rad(delta))) * 180 / pi
}
