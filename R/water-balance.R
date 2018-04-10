#' Calculate Water Balance using Thornthwaite method
#' @export
wb_seq <- function(.data, cad, ppt, etp) {

  ppt <- dplyr::enquo(ppt)
  etp <- dplyr::enquo(etp)
  cad <- dplyr::enquo(cad)

  vec_ppt <- dplyr::pull(.data, !!ppt)
  vec_etp <- dplyr::pull(.data, !!etp)
  vec_cad <- dplyr::pull(.data, !!cad)

  p_etp <- vec_ppt - vec_etp

  # neg = ifelse(p_etp < 0, p_etp, 0) %>% sum()
  # posi = ifelse(p_etp >= 0, p_etp, 0) %>% sum()
  #
  # arm <- ifelse(
  #   p_etp > 0,
  #
  # )

  #neg_acum <- vector("numeric", length(ppt))
  arm <- vector("numeric", length(p_etp))
  alt <- vector("numeric", length(p_etp))
  etr <- vector("numeric", length(p_etp))
  def <- vector("numeric", length(p_etp))
  ext <- vector("numeric", length(p_etp))

  wb_start <- FALSE

# i = 1
  for (i in seq_along(p_etp)) {

    if (!wb_start) {
      if (p_etp[i] > 0 & p_etp[i] > vec_cad[i]) {
        arm[i] <- vec_cad[i]
        alt[i] <- 0 #p_etp[i]
        etr[i] <- vec_etp[i]
        def[i] <- vec_etp[i] - etr[i]
        ext[i] <- p_etp[i] - alt[i]
        wb_start <- TRUE
      } else {
        arm[i] <- NA
        alt[i] <- NA
        etr[i] <- NA
        def[i] <- NA
        ext[i] <- NA
      }
      next()
    }

    if (p_etp[i] < 0) {
      arm_n <- arm[i - 1] * exp(p_etp[i] / vec_cad[i])
      arm[i] <- ifelse(arm_n < vec_cad[i], arm_n, vec_cad[i])
    } else {
      arm_n <- arm[i - 1] + p_etp[i]
      arm[i] <- ifelse(arm_n < vec_cad[i], arm_n, vec_cad[i])
    }

    alt[i] <- arm[i] - arm[i-1]
    if (p_etp[i] >= 0) {
      etr[i] <- vec_etp[i]
    }

    if (alt[i] < 0) {
      etr[i] <- vec_ppt[i] + abs(alt[i])
    }
    # else ara etr?

    def[i] <- vec_etp[i] - etr[i]

    if (arm[i] < vec_cad[i]) {
      ext[i] <- 0
    } else {
      ext[i] <- p_etp[i] - alt[i]
    }
  }

  out <- dplyr::data_frame(
    arm,
    alt,
    etr,
    def,
    ext
  ) %>%
  dplyr::mutate_all(round, digits = 1)

  bind_cols(.data, out)
}


# x <- p_etp
# i = 1
# guess_start <- function(x) {
#
#   #n = length(x)
#
#   for (i in seq_along(x)) {
#    guess[i] <- all(x[0:2 + i] > 0)
#   }
#
#   which(x, guess[1])
#
# }
#
# seq_cycle <- function(x, new) {
#
#   stopifnot(new %in% x)
#
#   if (x[1] == new) {
#     return(x)
#   }
#
#   n <- length(x)
#   i <- which(x == new)
#
#   x[c(i:n, 1:(i-1))]
# }
