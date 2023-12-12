
# TODO: edge case no tree yet trained, i = 0
ds.output_pred <- function(last_tr_tree, datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.data.frame(last_tr_tree)){
    stop("'last_tr_tree' needs to be an object of type 'data frame'.")
  }
  
  cally <- call("output_predDS", last_tr_tree)
  output <- DSI::datashield.assign.expr(datasources, "output_prediction", cally)
}