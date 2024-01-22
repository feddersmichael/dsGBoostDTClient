
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
  # TODO: Save if the best split contains NA values
  # TODO: Add gamma at the end.
  
  
  split_sums <- ds.summed_bins(leaves)
  
  # Now we can calculate the split score for all possibilities.
  # From all split scores we can then choose the best split.
  best_split <- data.frame(sp_sc = numeric(), feature = numeric(),
                           split_val = numeric(), cont_NA = numeric(),
                           weight_l = numeric(), weight_r = numeric())
  lambda <- reg_par[[1]]
  for (leaf in 1:length(split_sums)) {
    
    sums <- split_sums[[leaf]]
    # can be subtracted at the very end probably
    prev_sc <- sums$compl$grad^2 / (sums$compl$hess + lambda)
    
    # We write it saving based to check correctness first
    calc_spsc <- function(grad, hess){
      
      if (ncol(grad) == 4){
        split_val <- data.frame(spsc_NA_l = numeric(), spsc_NA_r = numeric())
        
        for (i in 1:nrow(grad)){
          left_split_NA <- grad$sum_L_NA[i]^2 / (hess$sum_L_NA[i] + lambda)
          left_split <- grad$sum_L[i]^2 / (hess$sum_L[i] + lambda)
          right_split_NA <- grad$sum_R_NA[i]^2 / (hess$sum_R_NA[i] + lambda)
          right_split <- grad$sum_R[i]^2 / (hess$sum_R[i] + lambda)
          
          split_val[i, ] <- c(left_split_NA + right_split + prev_sc,
                              left_split + right_split_NA + prev_sc)
        }
      }
      else {
        split_val <- data.frame(spsc = numeric())
        
        for (i in 1:nrow(grad)){
          left_split <- grad$sum_L[i]^2 / (hess$sum_L[i] + lambda)
          right_split <- grad$sum_R[i]^2 / (hess$sum_R[i] + lambda)
          
          split_val[i, ] <- c(left_split + right_split + prev_sc)
        }
      }
      
      return(split_val)
    }
    
    split_val <- mapply(calc_spsc, sums$grad, sums$hess)
    
    cont_NA <- NULL
    feature <- NULL
    split_pt <- NULL
    spsc <- 0
    weight_l <- NULL
    weight_r <- NULL
    
    for (i in 1:length(split_val)){
      for (j in 1:nrow(split_val[[i]])){
        if (ncol(split_val[[i]]) == 2){
          if (split_val[[i]]$spsc_NA_l[j] > spsc){
            spsc <- split_val[[i]]$spsc_NA_l[j]
            feature <- i
            cont_NA <- 1
            split_pt <- spp_cand[[i]][j]
            weight_l <- -sums[[1]][[i]]$sum_L_NA[j] / 
              (sums[[2]][[i]]$sum_L_NA[j])
            weight_r <- -sums[[1]][[i]]$sum_R[j] / (sums[[2]][[i]]$sum_R[j])
          }
          if (split_val[[i]]$spsc_NA_r[j] > spsc){
            spsc <- split_val[[i]]$spsc_NA_l[j]
            feature <- i
            cont_NA <- 2
            split_pt <- spp_cand[[i]][j]
            weight_l <- -sums[[1]][[i]]$sum_L[j] / (sums[[2]][[i]]$sum_L[j])
            weight_r <- -sums[[1]][[i]]$sum_R_NA[j] /
              (sums[[2]][[i]]$sum_R_NA[j])
          }
        }
        else {
          if (split_val[[i]]$spsc[j] > spsc){
            spsc <- split_val[[i]]$spsc[j]
            feature <- i
            cont_NA <- 0
            split_pt <- spp_cand[[i]][j]
            weight_l <- -sums[[1]][[i]]$sum_L[j] / (sums[[2]][[i]]$sum_L[j])
            weight_r <- -sums[[1]][[i]]$sum_R[j] / (sums[[2]][[i]]$sum_R[j])
          }
        }
      }
    }
    
    best_split[leaf, ] <- c(spsc, feature, split_pt, cont_NA, weight_l,
                            weight_r)
  } 
  # Finally we return our choice.
  return(best_split)
}