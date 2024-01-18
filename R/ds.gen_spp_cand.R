
#' Generate Splitting point candidates
#'
#' @param data_name The name under which the data is saved on the server.
#' @param cand_select A data frame defining the split-point generation for all
#' different types.
#' @param cand_hyper Hyperparameter for all candidate selection methods.
#' @param data_classes List of data class per column.
#' @param amt_spp Amount of splitting points per feature.
#' @param datasources DATASHIELD server connection.
#'
#' @return The created splitting points.
#' @export
<<<<<<< HEAD
ds.gen_spp_cand <- function(bounds_and_levels, data_classes, cand_select,
                            amt_spp){
=======
ds.gen_spp_cand <- function(data_name, cand_select, cand_hyper, data_classes,
                            amt_spp, datasources = NULL){
  # Idea: if we want to introduce other 'cand_select_mode' which needs different
  # parameters than 'amt_spp' and 'min_max' we ask for a general list 'parameters'
  # and then just pass the elements through 'parameters[[1]]', 'parameters[[2]]'
  # etc.
>>>>>>> b1be14c27b0c1d88085d21d398f80eb4225d34f7
  # Idea: add mode for logarithmic scale e.g. for uniform
  # TODO: implement iterative hessian
  
  
  cand_modes <- names(cand_select)
  
  if (!is.list(cand_select)){
    stop("'cand_select' needs to be an object of type 'list'.")
  }
  else if (!length(cand_select) == 2) {
    stop("'cand_select' is supposed to have two entries")
  }
  else if(!identical(cand_modes, c("numeric", "factor"))) {
    stop("The two data classes for which we generate splitting points are 'numeric' and 'factor'.")
  }
  
  supported_modes <- list(numeric = c("uniform", "loguniform", "ithess"),
                          factor = c("exact"))
  
  for (cur_mode in cand_modes) {
    if (!cand_select[[cur_mode]] %in% supported_modes[[cur_mode]]) {
      stop(paste0("'", cand_select[[cur_mode]], "' is not a supported split-point generating method for '", cur_mode, "'."))
    }
  }

  spp_cand <- list()
  
<<<<<<< HEAD
  for (feature in names(data_classes)) {
    
    if (data_classes[feature] == "numeric") {
      spp_cand[[feature]] <- ds.gen_numeric_spp_cand(bounds_and_levels[[feature]],
                                                     amt_spp[[feature]], cand_select$numeric)
    }
    else if (data_classes[feature] == "factor") {
      spp_cand[[feature]] <- ds.gen_factor_spp_cand(bounds_and_levels[[feature]],
                                                    amt_spp[[feature]], cand_select$factor)
    }
=======
  # To generate the splitting-point candidates we need to have min_max values
  # for each feature for which we want to create 'amt_spp' splitting points.

  # For each type of candidate-selection we call a different function.
  if (cand_select == "uniform"){
    
    min_max <- list()
    
    if (length(amt_spp) != length(min_max)) {
      stop("'amt_spp' and 'min_max' need to have the same length.")
    }
    
    unif_appl <- function(min_max, amt_spp) {
      stats::runif(amt_spp, min_max[1], min_max[2])
    }
    spp_cand <- mapply(unif_appl, min_max, amt_spp)
  }
  else if (cand_select == "ithess"){
    spp_cand <- ds.ithess_spp_cand()
>>>>>>> b1be14c27b0c1d88085d21d398f80eb4225d34f7
  }

  return(spp_cand)
}