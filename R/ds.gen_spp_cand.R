
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
#' @param new_num_spp Whether new numerical spp-point candidates shall be
#' generated.
#' @param split_method The used split-method.
#' @param selected_feat For which features we create splitting points.
#' @param datasources DATASHIELD server connection.
#'
#' @return The created splitting points.
#' @export

ds.gen_spp_cand <- function(data_name, bounds_and_levels, data_classes, amt_spp,
                            cand_select, add_par = NULL, new_num_spp = TRUE,
                            split_method = NULL, selected_feat = NULL,
                            datasources = NULL) {
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }
  
  # TODO: Add after weight update new hessians.
  if (cand_select[["numeric"]] == "ithess" && new_num_spp &&
      (split_method == "totally_random" || (split_method == "histograms" &&
                                            !is.null(selected_feat)))) {
    cally <- call("hessiansDS", data_name)
    hessians_list <- DSI::datashield.aggregate(datasources, cally)
    
    reduce_hessian <- function(S_1, S_2) {
      mapply(function(feature_1, feature_2){mapply(sum, feature_1, feature_2)},
             S_1, S_2, SIMPLIFY = FALSE)
    }
    
    add_par[["hessians"]] <- Reduce(reduce_hessian, hessians_list)
  }

  
  if (cand_select[["numeric"]] == "ithess") {
    hessians <- add_par[["hessians"]]
  }
  
  if (is.null(selected_feat)) {
    feature_choices <- names(data_classes)
  } else {
    feature_choices <- selected_feat
  }
  
  spp_cand <- add_par[["spp_cand"]]
  for (feature in feature_choices) {
    if (data_classes[[feature]] == "numeric") {
      if (new_num_spp) {
        if (cand_select[["numeric"]] == "ithess") {
          add_par_feat <- list(hessians = hessians[[feature]],
                          prev_spp_cand = spp_cand[[feature]])
        } else {
          add_par_feat <- NULL
        }
        spp_cand[[feature]] <- ds.gen_numeric_spp_cand(bounds_and_levels[[feature]],
                                                       amt_spp[[feature]],
                                                       cand_select[["numeric"]],
                                                       add_par_feat)
      }
    } else {
      spp_cand[[feature]] <- ds.gen_factor_spp_cand(length(bounds_and_levels[[feature]]),
                                                    amt_spp[[feature]], cand_select[["factor"]])
    }
  }
  
  save_list <- list(spp_cand = spp_cand)
  exist_check <- c(spp_cand = FALSE)
  ds.save_variables(data_name, save_list, exist_check, datasources)

  return(spp_cand)
}