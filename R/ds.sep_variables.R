
ds.sep_variables <- function(data_name, datsources = NULL){
  
  cally <- call("sep_variablesDS", data_name, "features")
  result <- DSI::datashield.assign.expr(datsources, paste0(data_name, 
                                                      "_features"), cally)
  
  
  cally <- call("sep_variablesDS", data_name, "output")
  result <- DSI::datashield.assign.expr(datsources, paste0(data_name, 
                                                      "_output"), cally)
  
}