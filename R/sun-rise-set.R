#' Sun rise
#'
#' @export
#'
sun_rise <- function(date, lon, lat) {
  z <- purrr::pmap_df(list(doy(date), lon, lat), sun_calc)

  z[["rise"]]
}


#' Sun set
#'
#' @export
#'
sun_set <- function(date, lon, lat) {
  z <- purrr::pmap_df(list(doy(date), lon, lat), sun_calc)

  z[["set"]]
}

sun_calc <- function(doy, lon, lat) {

  # copy from http://quantitative-ecology.blogspot.com.br/2007/10/approximate-sunrise-and-sunset-times.html

  ## doy is the day of year
  ## lat is latitude in decimal degrees
  ## lon is longitude in decimal degrees (negative == West)

  ##This method is copied from:
  ##Teets, D.A. 2003. Predicting sunrise and sunset times.
  ##  The College Mathematics Journal 34(4):317-321.

  ## At the default location the estimates of sunrise and sunset are within
  ## seven minutes of the correct times (http://aa.usno.navy.mil/data/docs/RS_OneYear.php)
  ## with a mean of 2.4 minutes error.

  ## Function to convert degrees to radians
  rad<-function(x)pi*x/180

  ##Radius of the earth (km)
  R=6378

  ##Radians between the xy-plane and the ecliptic plane
  epsilon=rad(23.45)

  ##Convert observer's latitude to radians
  L=rad(lat)

  ## Calculate offset of sunrise based on longitude (min)
  ## If lon is negative, then the mod represents degrees West of
  ## a standard time meridian, so timing of sunrise and sunset should
  ## be made later.
  timezone = -4*(abs(lon)%%15)*sign(lon)

  ## The earth's mean distance from the sun (km)
  r = 149598000

  theta = 2*pi/365.25*(doy-80)

  z.s = r*sin(theta)*sin(epsilon)
  r.p = sqrt(r^2-z.s^2)

  t0 = 1440/(2*pi)*acos((R-z.s*sin(L))/(r.p*cos(L)))

  ##a kludge adjustment for the radius of the sun
  that = t0+5

  ## Adjust "noon" for the fact that the earth's orbit is not circular:
  n = 720-10*sin(4*pi*(doy-80)/365.25)+8*sin(2*pi*doy/365.25)

  ## now sunrise and sunset are:
  sunrise = (n-that+timezone)/60
  sunset = (n+that+timezone)/60

  return(data.frame("rise" = sunrise, "set" = sunset))
}
