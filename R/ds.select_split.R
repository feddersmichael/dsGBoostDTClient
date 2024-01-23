
#' Choose the best split
#'
#' @param leaves Histogram-sums for all features and split-bins for each leaf.
#' @param spp_cand The candidates for a possible further split of the current
#' tree.
#' @param reg_par Regularisation parameters which specify the loss function.
#'
#' @return The best possible split under the possible ones.
#' @export
ds.select_split <- function(leaves, spp_cand, reg_par){
  
  # TODO: Structure which variables should be mentioned explicitly and which
  # ones only under a general variable-list
  # TODO: Add gamma at the end.
  
  split_sums <- ds.summed_bins(leaves)
  
  cont_NA <- list()
  for (i in 1:length(leaves)) {
    cont_NA[[i]] <- leaves[[i]]$cont_NA
  }
  
  best_split <- ds.calc_spsc(split_sums, spp_cand, reg_par, cont_NA)
  
  # Finally we return our choice.
  return(best_split)
}