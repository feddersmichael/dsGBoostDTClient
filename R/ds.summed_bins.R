
#' Sum up bins over all split-point candidates.
#'
#' @param leaves The histogram bins per leaf.
#'
#' @return The summed up bins.
ds.summed_bins <- function(leaves) {

  split_sums <- list()

  for (i in 1:length(leaves)) {
    leaf <- leaves[[i]]

    sums_per_leaf <- list(grad = list(), hess = list(),
                          compl = list(grad = NULL, hess = NULL))

    for (hist_mode in c("grad", "hess")) {

      histogram <- leaf[[hist_mode]]

      for (feature in names(histogram)) {

        bins <- histogram[[feature]]
        amt_bins <- length(bins)

        if (!is.na(histogram[[feature]]["NA"])) {
          bin_sums <- data.frame(sum_L_NA = numeric(), sum_R = numeric(),
                                 sum_L = numeric(), sum_R_NA = numeric())
          NA_elem <- histogram[[feature]]["NA"]
          sum_L <- bins[1]
          sum_R <- sum(bins[2:(amt_bins - 1)])

          bin_sums[1, ] <- c(sum_L + NA_elem, sum_R, sum_L, sum_R + NA_elem)

          for (j in 2:(amt_bins - 1)) {
            sum_L <- sum_L + bins[j]
            sum_R <- sum_R - bins[j]

            bin_sums[j, ] <- c(sum_L + NA_elem, sum_R, sum_L, sum_R + NA_elem)
          }
        }
        else {
          bin_sums <- data.frame(sum_L = numeric(), sum_R = numeric())

          sum_L <- bins[1]
          sum_R <- sum(bins[2:amt_bins])

          bin_sums[1, ] <- c(sum_L, sum_R)

          for (j in 2:(amt_bins - 1)) {
            sum_L <- sum_L + bins[j]
            sum_R <- sum_R - bins[j]

            bin_sums[j, ] <- c(sum_L, sum_R)
          }
        }

        sums_per_leaf[[hist_mode]][[feature]] <- bin_sums
      }
    }

    if (!is.na(leaf$grad[[1]]["NA"])) {
      sums_per_leaf$compl$grad <- sums_per_leaf$grad[[1]][1, 1] + 
                                       sums_per_leaf$grad[[1]][1, 2]
      sums_per_leaf$compl$hess <- sums_per_leaf$hess[[1]][1, 1] + 
                                       sums_per_leaf$hess[[1]][1, 2]
    }
    else {
      sums_per_leaf$compl$grad <- sums_per_leaf$grad[[1]][1, 1] +
                                  sums_per_leaf$grad[[1]][1, 2]
      sums_per_leaf$compl$hess <- sums_per_leaf$hess[[1]][1, 1] +
                                  sums_per_leaf$hess[[1]][1, 2]
    }

    split_sums[[i]] <- sums_per_leaf
  }

  return(split_sums)
}