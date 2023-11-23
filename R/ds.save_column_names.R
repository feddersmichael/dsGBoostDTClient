
# TODO: could be part of data check routine
ds.save_column_names <- function(data_name, datsources = NULL){
  
  cally <- call("save_column_namesDS", data_name)
  result <- DSI::datashield.assign.expr(datsources, "column_names", cally)
  
}