#' @export
#'
fill_reg <- function(.data, id, key, lon, lat, value) {

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
  for (i in seq_along(list_id_miss)) {

    # filter id's data for each loop
    df_i <- dplyr::filter(.data, (!!id) == list_id_miss[i])


    # calc the proportion between missing and filled values
    if (prop_miss(df_i, !!value) > 50) {
      # message or stop?
      message(paste("ID:", list_id_miss[i], "Prop:", prop_miss(df_i, !!value)))
    }


    ## precisa pegar mais de uma etação proxima!
    # get nearst id
    nxt <- nxt_id(.data, !!id, !!lon, !!lat, list_id_miss[i])


    # filter nearest id' data
    df_nxt <- dplyr::filter(.data, (!!id) == nxt)


    # calc coverage percent. trigger into call function??
    if (coverage(df_i, df_nxt, !!key, !!value) < 80) {
      # message or stop?
      p <- coverage(df_i, df_nxt, !!id, !!key, !!value)
      message(glue::glue("{list_id_miss[i]} and {nxt} don't have enoght coverage. Poor {p}%"))
    }


    # fit simple linear model
    lm_i <- fit_model(df_i, df_nxt, !!value)


    # test r2. trigger into call function??
    if (summary(lm_i)$r.squared < 0.8) {
      r2 <- round(summary(lm_i)$r.squared, 2)
      message(glue::glue("{list_id_miss[i]} and {nxt} don't have a good fit. Poor {r2}"))
    }


    # predict just NA value
    vec_pred_i <- predict_value(lm_i, df_i, df_nxt, !!value)

    # replace predic values into base data
    vec_id_i <- dplyr::pull(.data, !!id) == list_id_miss[i]
    .data[vec_id_i, dplyr::quo_name(value)] <- vec_pred_i
  }

  .data
}

#' @export
#'
is_complete <- function(.data, value) {
  # test if vector is fill
  value <- dplyr::enquo(value)

  vec <- dplyr::pull(.data, !!value)
  all(!is.na(vec))
}

#' @export
#'
id_miss <- function(.data, id, value) {
  id <- dplyr::enquo(id)
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(.data, !!value))

  k <- dplyr::pull(.data[vec, ], !!id)

  unique(k)
}

#' @export
#'
prop_miss <- function(.data, value) {
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(.data, !!value))
  sum(vec) / nrow(.data) * 100
}

summarise_group <- function(.data, ...) {
  group_by <- dplyr::quos(...)

  g <- dplyr::group_by(.data, !!!group_by)
  s <- dplyr::summarise(g)
  dplyr::ungroup(s)
}

#' @export
#'
nxt_id <- function(.data, id, lon, lat, target) {

  id <- dplyr::enquo(id)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)

  x <- unique(dplyr::select(.data, !!id, !!lon, !!lat))

  y <- search_nearest(x, !!id, !!lon, !!lat)

  dplyr::filter(y, (!!id) == target)[[2]]
}


#' @export
#'
coverage <- function(data_i, data_nxt, key, value) {
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)

  # key's missing value
  df_x <- dplyr::filter(
    dplyr::select(data_i, !!key, !!value), is.na(!!value)
  )

  # just key and value variables
  df_y <- dplyr::select(data_nxt, !!key, !!value)

  # date_time t_min.x t_min.y
  join <- dplyr::left_join(df_x, df_y, by = dplyr::quo_name(key))

  # vector of fill values
  fill_vec <- !is.na(dplyr::pull(join, 3))
  if (all(!fill_vec)) {
    return(0)
  }

  # coverage in percert
  sum(fill_vec) / nrow(join) * 100
}


#' @export
#'
fit_model <- function(data_i, data_nxt, value) {
  value <- dplyr::enquo(value)

  x <- dplyr::pull(data_nxt, !!value)
  y <- dplyr::pull(data_i, !!value)

  lm(y ~ x)
}

#' @export
#'
predict_value <- function(model, data_i, data_nxt, value) {
  value <- dplyr::enquo(value)

  x <- dplyr::pull(data_nxt, !!value)
  y <- dplyr::pull(data_i, !!value)

  vec <- is.na(y)

  pred <- as.numeric(predict(model, data.frame(x = x[vec])))

  y[is.na(y)] <- round(pred, 1)

  y
}
