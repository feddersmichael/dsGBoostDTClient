
# TODO: Checks for variables
ds.uniform_spp_cand <- function(data_name, amt_spp, min_max, seed = NULL){
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  unif_appl <- function(min_max, n) runif(n, min_max[1], min_max[2])
  
  spp_cand <- apply(X = min_max, MARGIN = 2, FUN = unif_appl, amt_spp)
  
  return(spp_cand)
  
}