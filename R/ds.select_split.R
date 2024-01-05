
# TODO: Structure which variables should be mentioned explicitly and which
# ones only under a general variable-list
# TODO: Save if the best split contains NA values
ds.select_split <- function(histograms, spp_cand){
  
  

  for (leaf in histograms){
    
    na_check <- function(bins, cand){
      return(length(bins) > length(cand))
    }
    NA_val <- mapply(na_check, leaf[[1]], spp_cand)
    
    
    grad_sums <- mapply(sums, leaf[[1]], NA_val)
    hess_sums <- mapply()
  }
  
  
  
  
  
  
  
  # Now we can calculate the split score for all possibilities.
  spscores <- ds.calc_spsc(histograms)
  
  # From all split scores we can now choose the best split.
  best_split <- ds.best_split()
  
  # Finally we return our choice.
  return(best_split)
}