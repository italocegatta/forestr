#' IDW for especific window
#'
#' @export
idw_interval_value <- function(ref_data, ref_key, ref_value, ref_lon, ref_lat,
                               tgt_lon, tgt_lat, start, end, fun, max_dist = 200) {

  #start_norm <- lubridate::round_date(start, "month")
  #end_norm <- lubridate::round_date(end, "month")

  ref_key <- dplyr::enquo(ref_key)
  ref_value <- dplyr::enquo(ref_value)
  ref_lon <- dplyr::enquo(ref_lon)
  ref_lat <- dplyr::enquo(ref_lat)

  window <- ref_data %>%
    dplyr::filter(!!ref_key >= start, !!ref_key <= end)

  z <- window %>%
    dplyr::mutate(dist = haversine(!!ref_lon, !!ref_lat, !!tgt_lon, !!tgt_lat)) %>%
    dplyr::filter(dist < max_dist) %>%
    dplyr::group_by(!!ref_key) %>%
    dplyr::summarise(
      .ref_value = sum(
        !!ref_value / dist, na.rm = TRUE) /
        sum(dist^-1, na.rm = TRUE)
    ) %>%
    dplyr::summarise(!!dplyr::quo_name(ref_value) := fun(.ref_value, na.rm = TRUE)) %>%
    as.numeric()

  return(z)
}

