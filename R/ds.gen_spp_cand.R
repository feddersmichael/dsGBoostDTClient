
#' Generate Splitting point candidates
#'
#' @param bounds_and_levels Bounds for numeric variables and level sets for
#' factor variables.
#' @param data_classes List of data class per column.
#' @param cand_select Which splitting-point candidate selection is used for
#' numeric and factor data.
#' @param amt_spp Amount of splitting points per feature.
#'
#' @return The created splitting points.
#' @export

ds.gen_spp_cand <- function(bounds_and_levels, data_classes, cand_select,
                            amt_spp){
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
  
  for (feature in names(data_classes)) {
    
    if (data_classes[feature] == "numeric") {
      spp_cand[[feature]] <- ds.gen_numeric_spp_cand(bounds_and_levels[[feature]],
                                                     amt_spp[[feature]], cand_select$numeric)
    }
    else if (data_classes[feature] == "factor") {
      spp_cand[[feature]] <- ds.gen_factor_spp_cand(bounds_and_levels[[feature]],
                                                    amt_spp[[feature]], cand_select$factor)
    }

  }

  return(spp_cand)
}