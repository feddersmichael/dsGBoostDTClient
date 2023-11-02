ds.data_class_numeric <- function(name, datasources = NULL){
  
  if (!is.character(name)){
    stop(paste0("name needs to be a character but is ", class(name)))
  }
  
  if (is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- call("data_class_numericDS", name)
  
  result <- DSI::datashield.aggregate(datasources, cally)
  
  return(result)
}