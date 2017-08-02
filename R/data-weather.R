#' Weather
#'
#' 10 days of measurement from INMET's automatic weather station
#'
#' @section Variables:
#'
#' \itemize{
#'
#'  \item \code{id}: Station.
#'  \item \code{id}: City.
#'  \item \code{id}: Longitude.
#'  \item \code{id}: Latitude.
#'  \item \code{id}: Altitude.
#'  \item \code{date}: Time of log in datetime format
#'  \item \code{t_max}: Maximun temperature.
#'  \item \code{t_min}: Minimum temperature.
#'  \item \code{rh_max}: Maximum relative humidity.
#'  \item \code{rh_min}: Minimum relative humidity.
#'  \item \code{dp_max}: Maximum dew point.
#'  \item \code{dp_min}: Minimum dew point.
#'  \item \code{ap_max}: Maximum atmospheric pressure.
#'  \item \code{ap_min}: Minimum atmospheric pressure.
#'  \item \code{ws}: Wind speed.
#'  \item \code{wg}: Wind gust.
#'  \item \code{wd}: Wind direction. radiation
#'  \item \code{rad}: Global radiation.
#'  \item \code{prec}: Precipitation.
#' }
#'
#' @docType data
#' @name weather
#' @usage weather
#' @format A \code{data frame} with 2,640 observations and 19 variables.
"weather"
