
#' Generate Splitting point candidates
#'
#' @param data_name The name under which the data is saved on the server.
#' @param bounds_and_levels Bounds for numeric variables and level sets for
#' factor variables.
#' @param data_classes List of data class per column.
#' @param amt_spp Amount of splitting points per feature.
#' @param cand_select Which splitting-point candidate selection is used for
#' numeric and factor data.
#' @param add_par Additional parameters for the iterative hessian mode.
#'
#' @return The created splitting points.
#' @export

ds.gen_spp_cand <- function(data_name, bounds_and_levels, data_classes, amt_spp,
                            cand_select, add_par = NULL) {

  if (!is.null(add_par[["datasources"]])) {
    cally <- call("hessiansDS", data_name, bounds_and_levels,
                  add_par[["spp_cand"]], data_classes)
    hessians_list <- DSI::datashield.aggregate(add_par[["datasources"]], cally)
    
    reduce_hessian <- function(S_1, S_2) {
      return(mapply(function(feature_1, feature_2){return(mapply(sum, feature_1, feature_2))},
                    S_1, S_2))
    }
    
    add_par[["hessians"]] <- Reduce(reduce_hessian, hessians_list)
  }

  spp_cand <- list()
  if (cand_select[["numeric"]] == "ithess") {
    hessians <- add_par[["hessians"]]
    prev_spp_cand <- add_par[["spp_cand"]]
  }
  for (feature in names(data_classes)) {
    if (data_classes[[feature]] == "numeric") {
      if (cand_select[["numeric"]] == "ithess") {
        add_par <- list(hessians = hessians[[feature]],
                        prev_spp_cand = prev_spp_cand[[feature]])
      }
      spp_cand[[feature]] <- ds.gen_numeric_spp_cand(bounds_and_levels[[feature]],
                                                     amt_spp[[feature]],
                                                     cand_select[["numeric"]],
                                                     add_par)
    } else {
      spp_cand[[feature]] <- ds.gen_factor_spp_cand(length(bounds_and_levels[[feature]]),
                                                    amt_spp[[feature]], cand_select[["factor"]])
    }
  }

  return(spp_cand)
}