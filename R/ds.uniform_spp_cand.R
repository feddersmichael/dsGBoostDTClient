
# TODO: Checks for variables
# TODO: amt_spp as vector to allow different amounts for each feature
#       -> might have to change output from matrix to list
ds.uniform_spp_cand <- function(data_name, amt_spp, min_max, seed = NULL){
  
  if (!is.null(seed)){
    set.seed(seed)
  }
  
  unif_appl <- function(min_max, n) runif(n, min_max[1], min_max[2])
  
  # spp_cand is a matrix of measures (amt_spp, dim(min_max)[[2]])
  spp_cand <- apply(X = min_max, MARGIN = 2, FUN = unif_appl, amt_spp, 
                    simplify = FALSE)
  
  return(spp_cand)
}