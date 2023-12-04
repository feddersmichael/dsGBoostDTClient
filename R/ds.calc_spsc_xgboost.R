
ds.calc_spsc_xgboost <- function(data_name, spp_cand, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  # for now assume that we just need g_i / h_i + lambda in total
  cally <- call("calc_spsc_xgboostDS", data_name, spp_cand, lambda)
  result <- DSI::datashield.aggregate(datasources, cally)
  
  
}