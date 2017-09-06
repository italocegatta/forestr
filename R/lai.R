#' Light area index
#'
#' @export
#'
lai <- function(int, ext, lat, lon, date, hour) {
  .doy <- doy(date)
  .declin <- declin(.doy)
  .te <- te(.doy)
  .to <- to(lon, .te)
  .zh <- zh(lat, .declin, hour, .to)
  .qg <- qg(.zh)
  .ext_mj <- ext_mj(ext)
  .r <- r(.ext_mj, .qg)
  .fb <- fb(.r, .qg)
  .tau <- tau(int, ext)
  .k <- k(.zh)

  ((1 - 1 / (2 * .k)) * .fb - 1) * log(.tau) / (0.86 * (1 - 0.47 * .fb))
}

#' day of the year
#'
#' @export
#'
doy <- function(date) {
  as.POSIXlt(date)$yday + 1
}

# Helpers -----------------------------------------------------------------

# solar declination
#
declin <- function(doy) {
  asin(0.39785 * sin(4.869 + (0.0172 * doy) + 0.03345 * sin(6.224 + 0.0172 * doy)))
}

# time equation
#
te <- function(doy) {
  a <- (279.575 + 0.986 * doy) * (pi / 180)
  (
    -104.7 * sin(a) + 596.2 * sin(2 * a) + 4.3 * sin(3 * a) - 12.7 * sin(4 * a) -
    429.9 * cos(a) - 2 * cos(2 * a) + 19.3 * cos(3 * a)
  ) / 3600
}

# minimum solar zenith
#
to <- function(lon, te) {
  lc <- (lon - (-45)) * (1 / 15)
  12 - lc - te
}

# zenith angle
#
zh <- function(lat, declin, hour, to) {
  a <- acos(
    (sin(lat * pi / 180) * sin(declin)) +
    (cos(lat * pi / 180) * cos(declin) * cos(0.2618 * (hour - to)))
  )
  a * (180 / pi)
}

# Extraterrestrial Solar Radiation
#
qg <- function(zh) {
  sc <- 1367 * 10 ^ (-6) * 3600 # solar constant
  z <- sc * cos(zh * pi / 180)
  z[z < 0] <- 0

  z
}

# conversion of μmols m-2 s-1 (Ceptometro) to MJ m-2 h-1 and correction of
# quantum efficiency for direct and diffuse radiation
#
ext_mj <- function(ext) {
  ifelse(
    ext >= 700,
    ((ext/4.57)*3600/1000000)/0.4348, # possui radiação direta e difusa
    ((ext/4.24)*3600/1000000)/0.4348  # possui radiacao difusa
  )
}

# ratio of incident solar radiation
#
r <- function(ext_mj, qg) {
  z <- ext_mj / qg
  z[ext_mj == 0] <- 0.2
  z[qg == 0] <- 0.2
  z[ext_mj / qg > 0.82] <- 0.82
  z[ext_mj / qg < 0.2]  <- 0.2

  z
}

# direct radiation fraction
#
fb <- function(r, qg) {
  z <- 1.395 + (r * (-14.43 + (r * (48.57 + (r * (-59.024 + (r * 24.835)))))))
  z[qg == 0] <- 0
  z[z < 0] <- 0

  z
}

# PAR radiation registered under the canopy
#
tau <- function(int, ext) {
  int / ext
}

# coefficient of light extinction
#
k <- function(zh, chi = 1) {
  chi <- 1 # Wang(2007)
  sqrt(chi ^ 2 + (tan(zh * pi / 180)) ^ 2) /
    (chi + 1.744 * (chi + 1.182)^(-0.733))
}
