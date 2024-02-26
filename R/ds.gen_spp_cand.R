
#' Generate Splitting point candidates
#'
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

ds.gen_spp_cand <- function(bounds_and_levels, data_classes, amt_spp,
                            cand_select, add_par = NULL) {

  supported_modes <- list(numeric = c("uniform", "loguniform", "ithess"),
                          factor = c("exact"))

  if (!cand_select[["numeric"]] %in% supported_modes[["numeric"]]) {
    stop(paste0("The mode '", cand_select[["numeric"]], "' is currently not supported to create split points for numeric features."))
  }

  if (!cand_select[["factor"]] %in% supported_modes[["factor"]]) {
    stop(paste0("The mode '", cand_select[["factor"]], "' is currently not supported to create split points for factor features."))
  }

  spp_cand <- list()
  for (feature in names(data_classes)) {
    if (data_classes[[feature]] == "numeric") {
      spp_cand[[feature]] <- ds.gen_numeric_spp_cand(bounds_and_levels[[feature]],
                                                     amt_spp[[feature]],
                                                     cand_select[["numeric"]],
                                                     list(add_par[[1]][[feature]],
                                                          add_par[[2]][[feature]]))
    } else {
      spp_cand[[feature]] <- ds.gen_factor_spp_cand(length(bounds_and_levels[[feature]]),
                                                    amt_spp[[feature]], cand_select[["factor"]])
    }
  }

  return(spp_cand)
}