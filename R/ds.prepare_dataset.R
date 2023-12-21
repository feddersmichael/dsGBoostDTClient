
# TODO: cooperation with data check

ds.prepare_dataset <- function(train_test_ratio, data_name, datsources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  # We save the column_names on the server -> might not be necessary
  # ds.save_column_names(data_name, datsources)
  
  # We split up the data into a training and test data set
  ds.create_data_split(data_name, train_test_ratio, datsources)
  
  # We separate features and output
  # TODO: merge with create_data_split + consider if necessary
  ds.sep_variables(data_name, datasources)
}