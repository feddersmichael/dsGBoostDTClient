
#' Creation of splitting-points for the numeric features
#'
#' @param bounds The bounds in which the splitting-points have to be.
#' @param amt_spp The amount of splitting-points which shall be created.
#' @param selection_method By which method the splitting-points shall be chosen.
#' @param add_par Additional parameters for the iterative hessian mode.
#'
#' @return The created splitting-points.
#' @export
ds.gen_numeric_spp_cand <- function(bounds, amt_spp, selection_method, add_par) {
  
  if (bounds[[2]] - bounds[[1]] < amt_spp * 1e-9) {
    amt_spp <- floor((bounds[[2]] - bounds[[1]]) * 1e-9)
  }
  
  if (selection_method == "uniform") {
    spp_cand <- (1:amt_spp) * ((bounds[[2]] - bounds[[1]]) / (amt_spp + 1)) +
                bounds[[1]]
  } else if (selection_method == "loguniform") {
    if (bounds[[1]] <= 0) {
      stop("We can only generate loguniform splitting point candidates for a strictly positive interval.")
    }
    log_bounds <- c(log(bounds))
    spp_cand <- exp(log_bounds[[1]] + (0:(amt_spp - 1)) * ((log_bounds[[2]] - log_bounds[[1]]) / (amt_spp - 1)))
  } else if (selection_method == "uniform_rand") {
    spp_cand <- sort(stats::runif(amt_spp, bounds[[1]], bounds[[2]]))
  } else if (selection_method == "ithess") {
    if (is.null(add_par)) {
      stop("'add_par' can't be NULL.")
    }
    
    hess_hist <- add_par[["hessians"]]
    prev_spp_cand <- add_par[["prev_spp_cand"]]
    # TODO: adapt amt_spp to NA-share?
    amt_hist_bins <- length(hess_hist) - 1
    theta <- sum(hess_hist[-(amt_hist_bins + 1)]) / (amt_spp + 1)
    bounds_and_spp_cand <- c(bounds[[1]], prev_spp_cand, bounds[[2]])
    spp_cand <- numeric()
    
    low_bnd <- 1
    upp_bnd <- 2
    split_cand <- list(middle = numeric(), hess = numeric())
    for (i in 1:(amt_hist_bins - 1)) {
      if (hess_hist[[i]] < theta) {
        upp_bnd <- upp_bnd + 1
        hess_hist[[i + 1]] <- hess_hist[[i]] + hess_hist[[i + 1]]
      } else {
        split_cand$middle <- c(split_cand$middle,
                               (bounds_and_spp_cand[[low_bnd]] +
                                bounds_and_spp_cand[[upp_bnd]]) / 2)
        split_cand$hess <- c(split_cand$hess, hess_hist[[i]])
        spp_cand <- c(spp_cand, bounds_and_spp_cand[[upp_bnd]])
        low_bnd <- upp_bnd
        upp_bnd <- upp_bnd + 1
      }
    }
    
    if (hess_hist[[amt_hist_bins]] >= theta) {
      split_cand$middle <- c(split_cand$middle,
                             (bounds_and_spp_cand[[low_bnd]] +
                                bounds_and_spp_cand[[upp_bnd]]) / 2)
      split_cand$hess <- c(split_cand$hess, hess_hist[[amt_hist_bins]])
    }
    
    split_cand$middle <- split_cand$middle[order(split_cand$hess,
                                                 decreasing = TRUE)]
    
    # adapt such that the middle point only gets added if the hessian is
    # double the size of the theta
    
    for (i in 1:length(split_cand$hess)) {
      if (length(spp_cand) >= amt_spp) {
        break
      }
      spp_cand <- c(spp_cand, split_cand$middle[[i]])
    }
    
    spp_cand <- sort(spp_cand)

    cur_amt_spp <- length(spp_cand)
    if (amt_spp > cur_amt_spp) {
      bounds_and_spp_cand <- c(bounds[[1]], spp_cand, bounds[[2]])
      new_spp <- sample.int((cur_amt_spp + 1), (amt_spp - cur_amt_spp),
                            replace = TRUE)
      fill_point <- numeric()
      for (i in new_spp) {
        fill_point <- c(fill_point, stats::runif(1, bounds_and_spp_cand[[i]],
                                                 bounds_and_spp_cand[[i + 1]]))
      }
      spp_cand <- sort(c(spp_cand, fill_point))
    }
  }
  
  dist_spp_cand <- c(spp_cand[[1]] - bounds[[1]])
  
  if (length(spp_cand) > 1) {
    for (i in 1:(length(spp_cand) - 1)) {
      dist_spp_cand[[i + 1]] <- spp_cand[[i + 1]] - spp_cand[[i]]
    }
  }
  
  dist_spp_cand[[length(spp_cand) + 1]] <- bounds[[2]] -  
    spp_cand[[length(spp_cand)]]
  
  i <- 1
  while (i < length(dist_spp_cand)) {
    if (dist_spp_cand[[i]] < 1e-9) {
      if ((dist_spp_cand[[i]] + dist_spp_cand[[i + 1]]) < 2e-9) {
        spp_cand <- spp_cand[-i]
        dist_spp_cand[[i]] <- dist_spp_cand[[i]] + dist_spp_cand[[i + 1]]
        dist_spp_cand <- dist_spp_cand[-(i + 1)]
      } else {
        if (length(spp_cand) == 1) {
          spp_cand[[1]] <- (bounds[[2]] + bounds[[1]]) / 2
          break
        } else {
          if (i == 1) {
            spp_cand[[1]] <- (spp_cand[[2]] + bounds[[1]]) / 2
            dist_spp_cand[[2]] <- spp_cand[[2]] - spp_cand[[1]]
          } else if (i == (length(dist_spp_cand) - 1)) {
            spp_cand[[i]] <- (spp_cand[[i - 1]] + bounds[[2]]) / 2
            break
          } else {
            spp_cand[[i]] <- (spp_cand[[i - 1]] + spp_cand[[i + 1]]) / 2
            dist_spp_cand[[i + 1]] <- spp_cand[[i + 1]] - spp_cand[[i]] 
          }
        }
        
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  return(spp_cand)
}