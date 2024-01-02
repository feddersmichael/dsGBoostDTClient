
ds.split_bins <- function(current_tree){
  
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
  
  cally <- call("split_binsDS", data_name, min_max, spp_cand, current_tree)
  hist <- DSI::datashield.aggregate(datasources, cally)
  
  # Now we introduce a help function to add up the histograms from the different
  # data servers.
  reduce_hist <- function(S_1, S_2){
    
    mapply(function(F_1, F_2){return(F_1 + F_2)}, S_1, S_2)
  }
  
  # first derivative histograms
  hist_1 <- Reduce(reduce_hist, hist[[1]])
  # second derivative histograms
  hist_2 <- Reduce(reduce_hist, hist[[2]])
  
  return(list(hist_1, hist_2))
}