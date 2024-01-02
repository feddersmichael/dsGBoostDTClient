
# TODO: Structure which variables should be mentioned explicitly and which
# ones only under a general variable-list
ds.select_split <- function(histograms, spp_cand){
  
  

  for (leaf in histograms){
    
  }
  
  
  
  
  
  
  
  # Now we can calculate the split score for all possibilities.
  spscores <- ds.calc_spsc(histograms)
  
  # From all split scores we can now choose the best split.
  best_split <- ds.best_split()
  
  # Finally we return our choice.
  return(best_split)
}