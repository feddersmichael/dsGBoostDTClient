
ds.calc_hist <- function(data_name, loss_function, data_type, curr_tree, spp_cand, 
                         datasources = NULL){
  
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
  
  if (!is.character(loss_function)){
    stop("'loss_function' needs to have data type 'character'.")
  }
  
  if (!is.character(data_type)){
    stop("'data_type' needs to have data type 'character'.")
  }
  
  if (!is.data.frame(curr_tree)){
    stop("'curr_tree' needs to be an object of type 'data frame'.")
  }
  
  if (!is.list(spp_cand)){
    stop("'spp_cand' needs to be an object of type 'list'.")
  }
  
  
  
  # We call the server to generate the histograms based on the splitting-point
  # candidates and the current tree structure
  cally <- call("calc_histDS", data_name, loss_function, data_type, curr_tree, 
                spp_cand)
  hist <- DSI::datashield.aggregate(datasources, cally)
  
}