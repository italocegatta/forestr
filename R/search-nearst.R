#' @export
#'
search_nearest  <- function(id, x, y, nearest = 1) {
  aux_class <- if (class(id) == "factor") {
    "character"
  } else {
    class(id)
  }

  m <- as.matrix(dist(data.frame(x, y)))
  rownames(m) <- id; colnames(m) <- id

  df <- as.data.frame(m)
  df <- dplyr::mutate(df, id = id)
  df <- tidyr::gather(df, candidate, .dist, -id)
  df <- dplyr::filter(df, .dist != 0)
  df <- dplyr::group_by(df, id)
  df <- dplyr::mutate(df, .rank = dplyr::min_rank(.dist))
  df <- dplyr::arrange(df, id, .rank)
  df <- dplyr::ungroup(df)
  df <- dplyr::filter(df, .rank <= nearest)
  df <- dplyr::select(df, -.rank)
  df <- dplyr::mutate(
      df,
      id = 'class<-'(id, aux_class),
      candidate = 'class<-'(candidate, aux_class)
    )

  df$candidate
}
