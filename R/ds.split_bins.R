
#' ds.split_bins
#'
#' @param data_name 
#' @param min_max 
#' @param current_tree 
#' @param spp_cand 
#' @param current_tree 
#' @param data_type 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.split_bins <- function(data_name, min_max, current_tree, spp_cand,
                          current_tree, data_type, datasources = NULL){
  
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