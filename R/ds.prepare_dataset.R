
ds.prepare_dataset <- function(train_ratio, data_name, datsources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  ds.save_column_names(data_name, datsources)
  
  ds.sep_variables(data_name, datsources)
  
  ds.create_data_split(train_ratio, datsources)
  
}