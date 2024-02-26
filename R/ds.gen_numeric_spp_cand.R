
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
  } else if (selection_method == "loguniform") {
    if (bounds[[1]] <= 0 || bounds[[2]] <= 0) {
      stop("We can only generate loguniform splitting point candidates for a strictly positive interval.")
    }
    log_bounds <- c(log(bounds))
    spp_cand <- sort(exp(stats::runif(amt_spp, log_bounds[[1]],
                                      log_bounds[[2]])))
  } else if (selection_method == "ithess") {
    hess_hist <- list()
    prev_spp_cand <- list()
    theta <- 0
    bounds <- c(bounds[[1]], prev_spp_cand, bounds[[2]])
    spp_cand <- c()
    
    low_bnd <- 1
    upp_bnd <- 2
    for (i in 1:(length(prev_spp_cand) - 1)) {
      if (hess_hist[[i]] < theta) {
        upp_bnd <- i + 1
        hess_hist[[i + 1]] <- hess_hist[[i]] + hess_hist[[i + 1]]
      } else {
        middle <- (bounds[[low_bnd]] + bounds[[upp_bnd]]) / 2
        spp_cand <- c(spp_cand, middle, bounds[[upp_bnd]])
        low_bnd <- upp_bnd
        upp_bnd <- upp_bnd + 1
      }
    }
    
    if (hess_hist[[length(prev_spp_cand)]] >= theta) {
      middle <- (bounds[[low_bnd]] + bounds[[upp_bnd]]) / 2
      spp_cand <- c(spp_cand, middle)
    }
    
    cur_amt_spp <- length(spp_cand)
    if (amt_spp > cur_amt_spp) {
      bounds <- c(bounds[[1]], spp_cand, bounds[[2]])
      new_spp <- sample.int(cur_amt_spp, (amt_spp - cur_amt_spp), replace = TRUE)
      fill_point <- c()
      for (i in new_spp) {
        fill_point <- c(fill_point, stats::runif(1, bounds[[i]], bounds[[i + 2]]))
      }
      spp_cand <- sort(c(spp_cand, fill_point))
    }
  }

  return(spp_cand)
}