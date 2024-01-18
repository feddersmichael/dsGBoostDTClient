
ds.gen_numeric_spp_cand <- function(bounds, amt_spp, selection_method) {

  spp_cand <- numeric()

  if (selection_method == "uniform") {

    spp_cand <- sort(stats::runif(amt_spp, bounds[1], bounds[2]))
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