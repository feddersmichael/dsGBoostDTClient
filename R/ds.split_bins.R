
#' Split the data into bins
#'
#' @param data_name The name under which the data is saved on the server.
#' @param min_max The maximum and minimum values of all features.
#' @param current_tree The tree which gets currently trained.
#' @param spp_cand The candidates for a possible further split of the current
#' tree.
#' @param data_type Denotes for each feature if it is numeric or (originally)
#' categorical.
#' @param datasources DATASHIELD server connection.
#'
#' @return The histogram sums for each bin for all features.
#' @export
ds.split_bins <- function(data_name, min_max, spp_cand, current_tree,
                          data_type, datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.list(spp_cand)){
    stop("'spp_cand' needs to be an object of type 'list'.")
  }
  
  cally <- call("split_binsDS", data_name, min_max, spp_cand, current_tree,
                data_type)
  hist <- DSI::datashield.aggregate(datasources, cally)
  
  # Now we introduce a help function to add up the histograms from the different
  # data servers.
  reduce_hist <- function(S_1, S_2){
    
    mapply(function(F_1, F_2){return(F_1 + F_2)}, S_1, S_2)
  }
  
  histograms_compl <- list()
  for (leaf in hist){
    # first derivative histograms
    hist_1 <- Reduce(reduce_hist, leaf[[1]])
    # second derivative histograms
    hist_2 <- Reduce(reduce_hist, leaf[[2]])
    histograms_compl <- append(histograms_compl, list(hist_1, hist_2))
  }

  return(histograms_compl)
}