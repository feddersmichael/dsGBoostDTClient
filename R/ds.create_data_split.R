
ds.create_data_split <- function(data_name, train_test_ratio, datsources){
  
  # We split up the dataset in a training and test part.
  cally <- call("create_data_splitDS", data_name, train_test_ratio)
  result <- DSI::datashield.assign.expr(datasources, 
                                      paste0(data_name, "_training_test_split"), 
                                      cally)
  
}