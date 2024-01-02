
# TODO: Structure which variables should be mentioned explicitly and which
# ones only under a general variable-list
ds.select_split <- function(current_tree, histograms, datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }

  
  
  
  
  
  
  
  
  # Now we can calculate the split score for all possibilities.
  spscores <- ds.calc_spsc(histograms)
  
  # From all split scores we can now choose the best split.
  best_split <- ds.best_split()
  
  # Finally we return our choice.
  return(best_split)
}