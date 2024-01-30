
#' Calculate the split score and choose the best one
#'
#' @param split_sums The histogram sums for all split bins.
#' @param spp_cand The split-point candidates for which the bins were 
#' calculated.
#' @param reg_par The regularisation parameters.
#' @param cont_NA Vector which numeric feature has additional NA values.
#'
#' @return The split with the best split score.
ds.calc_spsc <- function(split_sums, spp_cand, reg_par, cont_NA) {
  # TODO: What if highest spsc is 0?
  
  lambda <- reg_par[[1]]
  gamma <- reg_par[[2]]
  
  # Now we can calculate the split score for all possibilities.
  # From all split scores we can then choose the best split.
  opt_sp_per_leaf <- data.frame(sp_sc = numeric(), feature = numeric(),
                           split_val = numeric(), cont_NA = numeric(),
                           weight_l = numeric(), weight_r = numeric())
  
  
  for (i in 1:length(split_sums)) {
    
    sums <- split_sums[[i]]
    
    if ((sums[["compl"]][["hess"]] + lambda) == 0) {
      opt_sp_per_leaf[i, ] <- list(0, NULL, NULL, NULL, NULL, NULL)
      next
    }
    else {
      prev_sc <- sums[["compl"]][["grad"]]^2 / (sums[["compl"]][["hess"]] + lambda)
    }
    
    # We write it saving based to check correctness first
    grad <- sums[["grad"]]
    hess <- sums[["hess"]]
    split_val <- list()
    denom_check_l <- 0
    denom_check_r <- 0
    
    for (feature in names(spp_cand)) {
      
      if (cont_NA[[i]][feature]) {
        
        split_val[[feature]] <- data.frame(spsc_NA_l = numeric(),
                                           spsc_NA_r = numeric())
        for (j in 1:nrow(grad[[feature]])){
          
          denom_check_l <- hess[[feature]]$sum_L_NA[[j]] + lambda
          denom_check_r <- hess[[feature]]$sum_R[[j]] + lambda
          if (denom_check_l != 0 && denom_check_r != 0) {
            left_split_NA <- grad[[feature]]$sum_L_NA[[j]]^2 / denom_check_l
            right_split <- grad[[feature]]$sum_R[[j]]^2 / denom_check_r
          }
          else {
            left_split_NA <- 0
            right_split <- 0
          }
          
          denom_check_l <- hess[[feature]]$sum_L[[j]] + lambda
          denom_check_r <- hess[[feature]]$sum_R_NA[[j]] + lambda
          if (denom_check_l != 0 && denom_check_r != 0) {
            left_split <- grad[[feature]]$sum_L[[j]]^2 / denom_check_l
            right_split_NA <- grad[[feature]]$sum_R_NA[[j]]^2 / denom_check_r
          }
          else {
            left_split <- 0
            right_split_NA <- 0
          }
          split_val[[feature]][j, ] <- c(left_split_NA + right_split,
                                         left_split + right_split_NA)
        }
      }
      else {
        
        split_val[[feature]] <- data.frame(spsc = numeric())
        for (j in 1:nrow(grad[[feature]])){
          
          denom_check_l <- hess[[feature]]$sum_L[[j]] + lambda
          denom_check_r <- hess[[feature]]$sum_R[[j]] + lambda
          if (denom_check_l != 0 && denom_check_r != 0) {
            left_split <- grad[[feature]]$sum_L[[j]]^2 / denom_check_l
            right_split <- grad[[feature]]$sum_R[[j]]^2 / denom_check_r
          }
          else {
            left_split <- 0
            right_split <- 0
          }
          split_val[[feature]][j, ] <- c(left_split + right_split)
        }
      }
    }
    
    split_cont_NA <- 0
    split_feature <- ""
    split_pt <- 0
    spsc <- 0
    weight_l <- 0
    weight_r <- 0
    
    for (feature in names(split_val)){
      
      cur_split <- split_val[[feature]]
      if (cont_NA[[i]][[feature]]) {
        for (j in 1:nrow(cur_split)) {
          if (cur_split$spsc_NA_l[[j]] > spsc){
            spsc <- cur_split$spsc_NA_l[[j]]
            split_feature <- feature
            split_cont_NA <- 1
            split_pt <- spp_cand[[feature]][[j]]
            weight_l <- -sums[["grad"]][[feature]]$sum_L_NA[[j]] / 
                         (sums[["hess"]][[feature]]$sum_L_NA[[j]] + lambda)
            weight_r <- -sums[["grad"]][[feature]]$sum_R[[j]] / 
                         (sums[["hess"]][[feature]]$sum_R[[j]] + lambda)
          }
          if (cur_split$spsc_NA_r[[j]] > spsc){
            spsc <- cur_split$spsc_NA_r[[j]]
            split_feature <- feature
            split_cont_NA <- 2
            split_pt <- spp_cand[[feature]][[j]]
            weight_l <- -sums[["grad"]][[feature]]$sum_L[[j]] /
                         (sums[["hess"]][[feature]]$sum_L[[j]] + lambda)
            weight_r <- -sums[["grad"]][[feature]]$sum_R_NA[[j]] /
                         (sums[["hess"]][[feature]]$sum_R_NA[[j]] + lambda)
          }
        }
      }
      else {
        for (j in 1:nrow(cur_split)) {
          if (cur_split$spsc[[j]] > spsc) {
            spsc <- cur_split$spsc[[j]]
            split_feature <- feature
            split_cont_NA <- 0
            split_pt <- spp_cand[[feature]][j]
            weight_l <- -sums[["grad"]][[feature]]$sum_L[[j]] / 
                         (sums[["hess"]][[feature]]$sum_L[[j]] + lambda)
            weight_r <- -sums[["grad"]][[feature]]$sum_R[[j]] /
                         (sums[["hess"]][[feature]]$sum_R[[j]] + lambda)
          }
        }
      }
    }
    
    spsc <- (spsc - prev_sc) / 2 - gamma
    best_split <- list(spsc, split_feature, split_pt, split_cont_NA, weight_l,
                    weight_r)
    opt_sp_per_leaf[i, ] <- best_split
  } 

  # Finally we return our choice.
  return(opt_sp_per_leaf)
}