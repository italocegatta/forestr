#' @export
#'
fill_reg <- function(data, id, key, lon, lat, value) {
  id <- dplyr::enquo(id)
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)

  if (is_complete(data, !!value)) return(data)

  list_id_miss <- id_miss(data, !!id, !!value)

  for (i in seq_along(list_id_miss)) {
    df_i <- dplyr::filter(data, (!!id) == list_id_miss[i])

    nearest_i <- near_st(data, !!id, !!lon, !!lat, list_id_miss[i])

    df_nxt <- dplyr::filter(data, (!!id) == nearest_i)

    if (prop_miss(df_i, !!value) > 70) {
      stop("tentar IDW")
    }

    if (coverage(df_i, df_nxt, !!id, !!key, !!value) != 100) {
      #message(paste(list_id_miss[i], "and", nearest_i, "don't have coverage"))
    }

    lm_i <- fit_model(df_i, df_nxt, !!value)

    if (summary(lm_i)$r.squared < 0.8) {
      #message(paste(list_id_miss[i], "and", nearest_i, "don't have nice fit"))
    }

    vec_pred_i <- predict_value(lm_i, df_i, df_nxt, !!value)
    vec_id_i <- dplyr::pull(data, (!!id)) == list_id_miss[i]

    data[vec_id_i, dplyr::quo_name(value)] <- vec_pred_i
  }

  return(data)
}

#' @export
#'
is_complete <- function(data, value) {
  # test if vector is fill
  value <- dplyr::enquo(value)

  vec <- dplyr::pull(data, !! value)
  all(!is.na(vec))
}

#' @export
#'
id_miss <- function(data, id, value) {
  id <- dplyr::enquo(id)
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(data, !!value))

  k <- dplyr::pull(data[vec, ], !!id)

  unique(k)
}

#' @export
#'
prop_miss <- function(data, value) {
  value <- dplyr::enquo(value)

  vec <- is.na(dplyr::pull(data, !!value))
  sum(vec) / nrow(data) * 100
}

summarise_group <- function(data, ...) {
  group_by <- dplyr::quos(...)

  g <- dplyr::group_by(data, !!!group_by)
  s <- dplyr::summarise(g)
  dplyr::ungroup(s)
}

#' @export
#'
near_st <- function(data, id, lon, lat, target) {

  id <- dplyr::enquo(id)
  lon <- dplyr::enquo(lon)
  lat <- dplyr::enquo(lat)

  x <- unique(dplyr::select(data, !!id, !!lon, !!lat))

  y <- search_nearest(x, !!id, !!lon, !!lat)

  dplyr::filter(y, (!!id) == target)[[2]]
}


#' @export
#'
coverage <- function(data_i, data_nxt, id, key, value) {
  key <- dplyr::enquo(key)
  value <- dplyr::enquo(value)

  # missing values
  df_x <- dplyr::filter(
    dplyr::select(data_i, !!key, !!value), is.na(!!value)
  )

  # vector from next id
  df_y <- dplyr::select(data_nxt, !!key, !!value)

  # key values match
  join <- dplyr::left_join(df_x, df_y, by = dplyr::quo_name(key))

  # fill vector
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
  y <- dplyr::pull(data_i, !! value)

  vec <- is.na(y)

  pred <- as.numeric(predict(model, data.frame(x = x[vec])))

  y[is.na(y)] <- round(pred, 1)

  y
}
