
#' Split the data into bins
#'
#' @param data_name The name under which the data is saved on the server.
#' @param current_tree The tree which gets currently trained.
#' @param spp_cand The candidates for a possible further split of the current
#' tree.
#' @param bounds_and_levels The maximum and minimum values for numeric features
#' and levels for factor features.
#' @param data_classes Denotes for each feature if it is numeric or a factor.
#' @param datasources DATASHIELD server connection.
#'
#' @return The histogram sums for each bin for all features.
#' @export
ds.split_bins <- function(data_name, current_tree, spp_cand, bounds_and_levels,
                          data_classes, datasources = NULL) {

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  else if (!all(sapply(datasources, DSI:::.isDSConnection))) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }

  cally <- call("split_binsDS", data_name, bounds_and_levels, spp_cand,
                current_tree, data_classes)
  histogram_per_server <- DSI::datashield.aggregate(datasources, cally)

  histograms_per_leave <- list()
  amt_leaves <- length(histogram_per_server[[1]])

  reduce_hist <- function(S_1, S_2) {

    leaves <- list()

    for (i in 1:amt_leaves) {
      comb_histograms <- list()
      comb_histograms[["grad"]] <- list()
      comb_histograms[["hess"]] <- list()

      for (feature in names(data_classes)) {
        comb_histograms[["grad"]][[feature]] <- mapply(sum, S_1[[i]][["grad"]][[feature]],
                                                  S_2[[i]][["grad"]][[feature]])
        comb_histograms[["hess"]][[feature]] <- mapply(sum, S_1[[i]][["hess"]][[feature]],
                                                  S_2[[i]][["hess"]][[feature]])
      }
      leaves[[i]] <- comb_histograms
    }

    return(leaves)
  }

  histograms_per_leaf <- Reduce(reduce_hist, histogram_per_server)
  
  for (i in 1:amt_leaves) {
    
    cont_NA <- logical()
    for (feature in names(spp_cand)) {
      categories <- histograms_per_leaf[[i]][["grad"]][[feature]]
      if ("NA" %in% names(categories) && (categories["NA"] != 0)) {
        cont_NA[[feature]] <- TRUE
      }
      else {
        cont_NA[[feature]] <- FALSE
      }
    }
    
    histograms_per_leaf[[i]][["cont_NA"]] <- cont_NA
  }

  return(histograms_per_leaf)
}