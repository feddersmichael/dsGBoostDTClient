
#' Split the data into bins
#'
#' @param data_name The name under which the data is saved on the server.
#' @param bounds_and_levels The maximum and minimum values for numeric features
#' and levels for factor features.
#' @param current_tree The tree which gets currently trained.
#' @param spp_cand The candidates for a possible further split of the current
#' tree.
#' @param data_classes Denotes for each feature if it is numeric or a factor.
#' @param datasources DATASHIELD server connection.
#'
#' @return The histogram sums for each bin for all features.
#' @export
ds.split_bins <- function(data_name, bounds_and_levels, spp_cand, current_tree,
                          data_classes, datasources = NULL) {

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  else if (!all(sapply(datasources, DSI:::.isDSConnection))) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }

  if (!is.character(data_name)) {
    stop("'data_name' needs to have data type 'character'.")
  }

  if (!is.list(spp_cand)) {
    stop("'spp_cand' needs to be an object of type 'list'.")
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
      comb_histograms$grad <- list()
      comb_histograms$hess <- list()

      for (feature in names(data_classes)) {
        comb_histograms$grad[[feature]] <- mapply(sum, S_1[[i]][[1]][[feature]],
                                                  S_2[[i]][[1]][[feature]])
        comb_histograms$hess[[feature]] <- mapply(sum, S_1[[i]][[2]][[feature]],
                                                  S_2[[i]][[2]][[feature]])
      }
      leaves <- append(leaves, list(comb_histograms))
    }

    return(leaves)
  }

  histograms_per_leave <- Reduce(reduce_hist, histogram_per_server)

  return(histograms_per_leave)
}