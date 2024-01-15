
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
ds.gen_spp_cand <- function(data_name, cand_select, cand_hyper, data_classes,
                            amt_spp, datasources = NULL){
  # Idea: if we want to introduce other 'cand_select_mode' which needs different
  # parameters than 'amt_spp' and 'min_max' we ask for a general list 'parameters'
  # and then just pass the elements through 'parameters[[1]]', 'parameters[[2]]'
  # etc.
  # Idea: add mode for logarithmic scale e.g. for uniform
  # TODO: implement iterative hessian
  
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  cand_modes <- colnames(cand_select)
  
  if (!is.data.frame(cand_select)){
    stop("'cand_select' needs to be an object of type 'data frame'.")
  }
  else if (!identical(nrow(cand_select), 1) 
           || !identical(ncol(cand_select), 3)) {
    stop("'cand_select' is supposed to have exactly one row and 3 columns.")
  }
  else if(!identical(cand_modes, c("numeric", "logical", "factor"))) {
    stop("The three data classes for which we generate splitting points are 'numeric', 'logical' and 'factor'.")
  }
  
  supported_modes <- list(numeric = c("uniform", "loguniform", "ithess"),
                          logical = c("exact"), factor = c("exact"))
  
  for (cur_mode in cand_modes) {
    if (!cand_select[[cur_mode]][1] %in% supported_modes[[cur_mode]]) {
      stop(paste0("'", cand_select[[cur_mode]][1], "' is not a supported split-point generating method for '", cur_mode, "'."))
    }
  }
  
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
  }
  
  return(spp_cand)
}