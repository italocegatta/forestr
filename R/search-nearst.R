#' @export
#'
search_nearest  <- function(data, id, x, y) {
  id <- dplyr::enquo(id)
  x <- dplyr::enquo(x)
  y <- dplyr::enquo(y)

  id_vec = dplyr::pull(data, !!id)
  id_class <- class(id_vec)

  aux_class <- if (id_class == "factor") {
    "character"
  } else {
    id_class
  }

  m <- as.matrix(dist(dplyr::select(data, !!x, !!y)))
  rownames(m) <- id_vec; colnames(m) <- id_vec

  df <- as.data.frame(m)
  df <- tibble::rownames_to_column(df)
  df <- tidyr::gather(df, candidate, .dist, -rowname)
  df <- dplyr::filter(df, .dist != 0)
  df <- dplyr::group_by(df, rowname)
  df <- dplyr::mutate(df, .rank = dplyr::min_rank(.dist))
  df <- dplyr::arrange(df, rowname, .rank)
  df <- dplyr::ungroup(df)
  df <- dplyr::filter(df, .rank <= 10) # 10 mais proximos
  df <- dplyr::select(df, -.rank)
  df <-dplyr::mutate(
    df,
    !!dplyr::quo_name(id) := 'class<-'(rowname, aux_class),
    nxt = 'class<-'(candidate, aux_class)
  )

  dplyr::select(df, !!id, nxt)
}
