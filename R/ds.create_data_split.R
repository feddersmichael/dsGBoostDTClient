
# TODO: Check for integer seed
ds.create_data_split <- function(data_name, train_test_ratio, datsources, 
                                 seed = NULL){
  
  # We split up the dataset in a training and test part. If reproducibility is
  # desired we can use a seed.
  cally <- call("create_data_splitDS", data_name, train_test_ratio, seed)
  result <- DSI::datashield.assign.expr(datasources, 
                                      paste0(data_name, "_training_test_split"), 
                                      cally)
  
}