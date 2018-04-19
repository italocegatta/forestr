
idw_interval_value <- function(ref_data, ref_key, ref_value, ref_lon, ref_lat,
                               tgt_lon, tgt_lat, start, end, min_dist = 100) {

  start_norm <- start - lubridate::mday(start) + 1
  end_norm <- end - lubridate::mday(end) + 1

  start_frac <- 1 - lubridate::mday(start) / lubridate::days_in_month(start) %>% as.vector()
  end_frac <- lubridate::mday(end) / lubridate::days_in_month(end) %>% as.vector()


  ref_key <- dplyr::enquo(ref_key)
  ref_value <- dplyr::enquo(ref_value)

  window <- ref_data %>%
    dplyr::filter(ref_key >= start_norm, ref_key <= end_norm)

  window[window$ref_key == start_norm, ]$ref_value <-
    window[window$ref_key == start_norm, ]$ref_value * start_frac

  window[window$ref_key == end_norm, ]$ref_value <-
    window[window$ref_key == end_norm, ]$ref_value * end_frac

  z <- window %>%
    dplyr::mutate(dist = haversine(ref_lon, ref_lat, tgt_lon, tgt_lat)) %>%
    dplyr::filter(dist < min_dist) %>%
    dplyr::group_by(ref_key) %>%
    dplyr::summarise(
      .ref_value = sum(
        ref_value / dist, na.rm = TRUE) /
        sum(dist^-1, na.rm = TRUE)
    ) %>%
    dplyr::summarise(ref_value := sum(.ref_value, na.rm = TRUE)) %>%
    as.numeric()

  return(z)
}

