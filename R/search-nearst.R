search_nearest_st <- function(id, x, y, nearest) {
  .class <- if (class(id) == "factor") {
    "character"
  } else {
    class(id)
  }

  m <- as.matrix(dist(data.frame(x, y)))

  rownames(m) <- id; colnames(m) <- id

  z <- as.data.frame(m) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(id = id) %>%
    tidyr::gather(candidate, .dist, -id) %>%
    dplyr::filter(.dist != 0) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(.rank = dplyr::min_rank(.dist)) %>%
    dplyr::arrange(id, .rank) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.rank <= nearest) %>%
    dplyr::select(-.rank) %>%
    dplyr::mutate(
      id = 'class<-'(id, .class),
      candidate = 'class<-'(candidate, .class)
    )

  return(z$candidate)
}
