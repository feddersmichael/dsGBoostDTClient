
ds.output_pred <- function(data_name, last_tr_tree, datasources = NULL){
  
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
  
  
  # If 'last_tr_tree' is NULL we initialise the predicted output with 0's.
  if (is.null(last_tr_tree)){
    cally <- call("output_pred_initDS", data_name)
    output <- DSI::datashield.assign.expr(datasources, 
                                          paste0(data_name, "_training"), cally)
  }
  else {
    if (!is.data.frame(last_tr_tree)){
      stop("'last_tr_tree' needs to be an object of type 'data frame'.")
    }
    
    cally <- call("output_predDS", data_name, last_tr_tree)
    output <- DSI::datashield.assign.expr(datasources, 
                                          paste0(data_name, "_training"), cally)
  }
  
}