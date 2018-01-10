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

wb_seq <- function(.data, cad, prec, etp) {

  prec <- dplyr::enquo(prec)
  etp <- dplyr::enquo(etp)
  cad <- dplyr::enquo(cad)
  p_etp <- pull(.data, !!prec) - pull(.data, !!etp)

  # neg = ifelse(p_etp < 0, p_etp, 0) %>% sum()
  # posi = ifelse(p_etp >= 0, p_etp, 0) %>% sum()
  #
  # arm <- ifelse(
  #   p_etp > 0,
  #
  # )

  #neg_acum <- vector("numeric", length(prec))
  arm <- vector("numeric", length(prec))
  alt <- vector("numeric", length(prec))
  etr <- vector("numeric", length(prec))
  def <- vector("numeric", length(prec))
  ext <- vector("numeric", length(prec))

  wb_start <- FALSE

# i = 1
  for (i in seq_along(prec)) {

    if (!wb_start) {
      if (p_etp[i] > 0 & p_etp[i] > cad[i]) {
        arm[i] <- cad[i]
        alt[i] <- 0 #p_etp[i]
        etr[i] <- etp[i]
        def[i] <- etp[i] - etr[i]
        ext[i] <- p_etp[i] - alt[i]
        wb_start <- TRUE
      } else {
        arm[i] <- NA
        alt[i] <- NA
      }
      next()
    }

    if (p_etp[i] < 0) {
      arm_n <- arm[i - 1] * exp(p_etp[i] / cad[i])
      arm[i] <- ifelse(arm_n < cad[i], arm_n, cad[i])
    } else {
      arm_n <- arm[i - 1] + p_etp[i]
      arm[i] <- ifelse(arm_n < cad[i], arm_n, cad[i])
    }

    alt[i] <- arm[i] - arm[i-1]
    if (p_etp[i] >= 0) {
      etr[i] <- etp[i]
    }

    if (alt[i] < 0) {
      etr[i] <- prec[i] + abs(alt[i])
    }
    # else ara etr?

    def[i] <- etp[i] - etr[i]

    if (arm[i] < cad[i]) {
      ext[i] <- 0
    } else {
      ext[i] <- p_etp[i] - alt[i]
    }
  }

  data_frame(
    prec,
    etp,
    p_etp,
    arm,
    alt,
    etr,
    def,
    ext
  ) %>%
  mutate_all(round, digits = 1)
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
