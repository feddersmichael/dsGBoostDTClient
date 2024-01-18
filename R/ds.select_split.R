
#' Choose the best split
#'
#' @param histograms Histogram-sums for all features and split-bins.
#' @param spp_cand The candidates for a possible further split of the current
#' tree.
#' @param reg_par Regularisation parameters which specify the loss function.
#'
#' @return The best possible split under the possible ones.
#' @export
ds.select_split <- function(histograms, spp_cand, reg_par){
  
  # TODO: Structure which variables should be mentioned explicitly and which
  # ones only under a general variable-list
  # TODO: Save if the best split contains NA values
  # TODO: Add gamma at the end.
  split_sums <- list()
  
  for (leaf in histograms){
    
    na_check <- function(bins, cand){
      return(length(bins) > length(cand))
    }
    NA_val <- mapply(na_check, leaf[[1]], spp_cand)
    
    grad_sums <- list()
    hess_sums <- list()
    
    for (feature in names(spp_cand)) {
      
      amt_bins <- length(leaf$grad[[feature]])
      
      if (!is.null(leaf$grad[[feature]]["NA"])) {
        
        bin_sums_grad <- data.frame(sum_L_NA = numeric(), sum_R = numeric(),
                               sum_L = numeric(), sum_R_NA = numeric())
        NA_elem_grad <- leaf$grad[[feature]]["NA"]
      }
      else {
        
      }
    }
    
    bin_sums <- function(bins, cont_NA){
      if (cont_NA){
        bin_sums <- data.frame(sum_L_NA = numeric(), sum_R = numeric(),
                               sum_L = numeric(), sum_R_NA = numeric())
        
        amt_bins <- length(bins)
        NA_elem <- bins[amt_bins]
        sum_L <- bins[1]
        sum_R <- sum(bins[2:amt_bins-1])
        
        bin_sums$sum_L[1] <- sum_L
        bin_sums$sum_L_NA[1] <- sum_L + NA_elem
        bin_sums$sum_R[1] <- sum_R
        bin_sums$sum_R_NA[1] <- sum_R + NA_elem
        
        for (i in 2:amt_bins-1){
          sum_L <- sum_L + bins[i]
          sum_R <- sum_R - bins[i]
          
          bin_sums$sum_L[i] <- sum_L
          bin_sums$sum_L_NA[i] <- sum_L + NA_elem
          bin_sums$sum_R[i] <- sum_R
          bin_sums$sum_R_NA[i] <- sum_R + NA_elem
        }
      }
      else {
        bin_sums <- data.frame(sum_L = numeric(), sum_R = numeric())
        
        amt_bins <- length(bins)
        sum_L <- bins[1]
        sum_R <- sum(bins[2:amt_bins-1])
        
        bin_sums$sum_L[1] <- sum_L
        bin_sums$sum_R[1] <- sum_R
        
        for (i in 2:amt_bins-1){
          sum_L <- sum_L + bins[i]
          sum_R <- sum_R - bins[i]
          
          bin_sums$sum_L[i] <- sum_L
          bin_sums$sum_R[i] <- sum_R
        }
      }
    }
    
    grad_sums <- mapply(bin_sums, leaf[[1]], NA_val)
    hess_sums <- mapply(bin_sums, leaf[[2]], NA_val)
    compl_sum <- list()
    compl_sum[[1]] <- grad_sums[[1]][[1]] + grad_sums[[1]][[2]]
    compl_sum[[2]] <- hess_sums[[1]][[1]] + hess_sums[[1]][[2]]
    
    split_sums <- append(split_sums, list(grad_sums, hess_sums, compl_sum))
  }
  
  # Now we can calculate the split score for all possibilities.
  
  
  # From all split scores we can now choose the best split.
  best_split <- data.frame(sp_sc = numeric(), feature = numeric(),
                           split_val = numeric(), cont_NA = numeric(),
                           weight_l = numeric(), weight_r = numeric())
  lambda <- reg_par[[1]]
  for (sums in split_sums){
    # can be subtracted at the very end probably
    prev_sc <- sums[[3]][[1]]^2 / (sums[[3]][[2]] + lambda)
    
    # We write it saving based to check correctness first
    calc_spsc <- function(grad, hess, prev_sc){
      
      if (ncol(grad) == 4){
        split_val <- data.frame(spsc_NA_l = numeric(), spsc_NA_r = numeric())
        
        for (i in 1:length(grad)){
          left_split_NA <- grad$sum_L_NA[i]^2 / (hess$sum_L_NA[i] + lambda)
          left_split <- grad$sum_L[i]^2 / (hess$sum_L[i] + lambda)
          right_split_NA <- grad$sum_R_NA[i]^2 / (hess$sum_R_NA[i] + lambda)
          right_split <- grad$sum_R[i]^2 / (hess$sum_R[i] + lambda)
          split_val[[1]][i] <- left_split_NA + right_split + prev_sc
          split_val[[2]][i] <- left_split + right_split_NA + prev_sc
        }
      }
      else {
        split_val <- data.frame(spsc = numeric())
        
        for (i in 1:length(grad)){
          left_split <- grad$sum_L[i]^2 / (hess$sum_L[i] + lambda)
          right_split <- grad$sum_R[i]^2 / (hess$sum_R[i] + lambda)
          split_val[[1]][i] <- left_split + right_split + prev_sc
        }
      }
      
      return(split_val)
    }
    
    split_val <- mapply(calc_spsc, sums[[1]], sums[[2]],
                        MoreArgs = list(prev_sc = prev_sc))
    
    cont_NA <- NULL
    feature <- NULL
    split_pt <- NULL
    spsc <- 0
    weight_l <- NULL
    weight_r <- NULL
    
    for (i in length(split_val)){
      for (j in nrow(split_val[[i]])){
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
    best_split <- rbind(best_split, c(spsc, feature, split_pt, cont_NA,
                                      weight_l, weight_r))
  } 
  # Finally we return our choice.
  return(best_split)
}