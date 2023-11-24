
ds.sep_variables <- function(data_name, datsources = NULL){
  
  cally <- call("sep_variablesDS", data_name)
  result <- DSI::datashield.assign.expr(datsources, paste0(data_name, 
                                                      "_sep"), cally)
  
}