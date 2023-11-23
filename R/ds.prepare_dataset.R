
# TODO: cooperation with data check
ds.prepare_dataset <- function(train_ratio, data_name, datsources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  # We save the column_names on the server -> might not be necessary
  ds.save_column_names(data_name, datsources)
  
  # We separate features and output
  # could maybe be done more efficient with specific function ds.sep_variables
  dsBaseClient::ds.dataFrameSubset(data_name)
  
  ds.create_data_split(train_ratio, datsources)
  
}