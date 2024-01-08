
#' Create Splitting-point candidates through uniform distribution
#'
#' @param amt_spp The amount of splitting points per feature.
#' @param min_max The boundaries of every feature.
#' @param seed If wanted a seed for the random sample.
#'
#' @return The Splitting-point candidates.
#' @export
ds.uniform_spp_cand <- function(amt_spp, min_max, seed = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (!is.null(seed)){
    if (!is.integer(seed)){
      stop("'seed' needs to have data type 'integer'.")
    }
    else {
      set.seed(seed)
    }
  }
  
  if (!is.list(min_max)){
    stop("'min_max' needs to be an object of type 'list'.")
  }
  
  if (!is.list(amt_spp)){
    stop("'amt_spp' needs to be an object of type 'list'.")
  }
  
  # To generate the splitting-point candidates we need to have min_max values
  # for each feature for which we want to create 'amt_spp' splitting points.
  if (!(length(amt_spp) == length(min_max))){
    stop("'amt_spp' and 'min_max' need to have the same length.")
  }
  
  # This is a help function to create all splitting-point candidates at once 
  # through 'mapply'.
  unif_appl <- function(min_max, amt_spp) {
    stats::runif(amt_spp, min_max[1], min_max[2])
  }
  
  # 'spp_cand' is a list of length 'length(amt_spp)' and the element at place i 
  # contains 'amt_spp[[i]]' splitting-pint candidates.
  spp_cand <- mapply(unif_appl, min_max, amt_spp)
  
  return(spp_cand)
}