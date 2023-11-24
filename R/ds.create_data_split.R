
ds.create_data_split <- function(data_name, train_ratio, datsources){
  
  cally <- call("creat_data_splitDS", data_name, train_ratio, )
  result <- DSI::datashield.assign.expr(datasources, paste0(data_name, "_feature_´test"))
}