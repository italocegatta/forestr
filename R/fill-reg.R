#' Fill gap with linear regression
#'
#' @export
#'
fill_reg <- function(.data, id, key, lon, lat, value, min_coverage = 80, max_iter = 10, min_r2 = 0.8) {

  id <- dplyr::enquo(id)
  key <- dplyr::enquo(key)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)
  value <- dplyr::enquo(value)

  # test if there is no missing values
  if (is_complete(.data, !!value)) return(.data)

  # find id that have missing values
  list_id_miss <- id_miss(.data, !!id, !!value)

  # i = 1
  # i = 2
  # i = 3
  for (i in seq_along(list_id_miss)) {

    # filter id's data for each loop
    df_i <- dplyr::filter(.data, (!!id) == list_id_miss[i])

    prop_miss_i <- prop_miss(df_i, !!value)
    # calc the proportion between missing and filled values
    if (prop_miss_i > 50) {
      # message or stop?
      message(glue::glue("id: {list_id_miss[i]}, missing {round(prop_miss_i, 2)}%"))
    }

    # get nearst id
    nxts <- nxt_id(.data, !!id, !!lon, !!lat, list_id_miss[i])
    if (length(nxts) < max_iter) {
      n_iter <- nxts
    } else {
      n_iter <- max_iter
    }

    j_r2 <- tibble::tibble(j = seq_len(n_iter), r2  = NA)
    for (j in seq_len(n_iter)) {

      # filter nearest id' data
      df_nxt_j <- dplyr::filter(.data, (!!id) == nxts[j])

      # calc coverage percent. trigger into call function??
      if (coverage(df_i, df_nxt_j, !!key, !!value) < min_coverage) next()

      # fit simple linear model
      lm_i <- fit_model(df_i, df_nxt_j, !!key, !!value)
      j_r2$r2[j] <- summary(lm_i)$r.squared

      # choose better fit
      if (j == max(n_iter)) {
        jj <- which.max(j_r2$r2)

        df_nxt_j <- dplyr::filter(.data, (!!id) == nxts[jj])
        lm_i <- fit_model(df_i, df_nxt_j, !!key, !!value)
        r2 <- round(summary(lm_i)$r.squared, 2)

        if (summary(lm_i)$r.squared < min_r2) {
          lm_i <- NULL
          message(glue::glue("{list_id_miss[i]}: r2 = {round(r2, 3)}"))
        }
      }
    }

    if (is.null(lm_i)) {
      next()
    }

    # predict just NA value
    vec_pred_i <- predict_value(lm_i, df_i, df_nxt_j, !!value)

    # replace predic values into base data
    vec_id_i <- dplyr::pull(.data, !!id) == list_id_miss[i]
    .data[vec_id_i, dplyr::quo_name(value)] <- vec_pred_i
    }

  .data
}
