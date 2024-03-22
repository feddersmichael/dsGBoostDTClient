
#' Choose the best split
#'
#' @param leaves Histogram-sums for all features and split-bins for each leaf.
#' @param spp_cand The candidates for a possible further split of the current
#' tree.
#' @param data_classes Data class for all features.
#' @param reg_par Regularisation parameters which specify the loss function.
#'
#' @return The best possible split under the possible ones.
#' @export
ds.select_split <- function(leaves, spp_cand, data_classes, reg_par) {

  # TODO: Structure which variables should be mentioned explicitly and which
  # ones only under a general variable-list

  split_sums <- ds.summed_bins(leaves, data_classes)

  cont_NA <- list()
  for (i in seq_along(leaves)) {
    cont_NA[[i]] <- leaves[[i]]$cont_NA
  }
  
  if (length(leaves) == 1) {
    reg_par[["first_leaf"]] <- 1
  }
  
  best_split <- ds.calc_spsc(split_sums, spp_cand, reg_par, cont_NA)

  # Finally we return our choice.
  return(best_split)
}