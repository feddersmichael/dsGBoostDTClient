
#' Split the data into bins
#'
#' @param data_name The name under which the data is saved on the server.
#' @param current_tree The tree which gets currently trained.
#' @param data_classes Denotes for each feature if it is numeric or a factor.
#' @param datasources DATASHIELD server connection.
#'
#' @return The histogram sums for each bin for all features.
#' @export
ds.split_bins <- function(data_name, current_tree, data_classes,
                          datasources = NULL) {

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }
  
  cally <- call("saveleafDS", data_name, current_tree)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_leaves"), cally)
  
  cally <- call("split_binsDS", data_name)
  histogram_per_server <- DSI::datashield.aggregate(datasources, cally)

  histograms_per_leaf <- list()
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
    for (feature in names(data_classes)) {
      categories <- histograms_per_leaf[[i]][["grad"]][[feature]]
      if ("NA" %in% names(categories) && data_classes[[feature]] == "numeric" &&
          (categories["NA"] != 0)) {
        cont_NA[[feature]] <- TRUE
      } else {
        cont_NA[[feature]] <- FALSE
      }
    }

    histograms_per_leaf[[i]][["cont_NA"]] <- cont_NA
  }

  return(histograms_per_leaf)
}