
# TODO: Structure which variables should be mentioned explicitly and which
# ones only under a general variable-list
# TODO: Save if the best split contains NA values
ds.select_split <- function(histograms, spp_cand, reg_par){
  
  split_sums <- list()
  
  for (leaf in histograms){
    
    na_check <- function(bins, cand){
      return(length(bins) > length(cand))
    }
    NA_val <- mapply(na_check, leaf[[1]], spp_cand)
    
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
  best_split <- ds.best_split()
  
  # Finally we return our choice.
  return(best_split)
}