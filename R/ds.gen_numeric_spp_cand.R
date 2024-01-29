
#' Creation of splitting-points for the numeric features
#'
#' @param bounds The bounds in which the splitting-points have to be.
#' @param amt_spp The amount of splitting-points which shall be created.
#' @param selection_method By which method the splitting-points shall be chosen.
#'
#' @return The created splitting-points.
#' @export
ds.gen_numeric_spp_cand <- function(bounds, amt_spp, selection_method) {

  if (selection_method == "uniform") {
    spp_cand <- sort(stats::runif(amt_spp, bounds[[1]], bounds[[2]]))
  }
  else if (selection_method == "loguniform") {
    stop("Please come back later.")
  }
  else if (selection_method == "percentile") {
    stop("Please come back later.")
  }
  else if (selection_method == "ithess") {
    stop("Please come back later.")
  }

  return(spp_cand)
}