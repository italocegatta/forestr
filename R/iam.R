#' IAM
#' @export
iam <- Vectorize(
  function(ppt, etp) {
    if (ppt < etp) {
      (ppt / etp) - 1
    } else {
      1 - (etp / ppt)
    }
  },
  vectorize.args = c("ppt", "etp")
)

