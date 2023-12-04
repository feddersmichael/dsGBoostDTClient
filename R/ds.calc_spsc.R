
# sth like c(ssc, feature_number, split_number, origin_split)
# obj_mode like 1st or 2nd order taylor, which regularization
# TODO: What if data is too big to load into the environment all at once?
ds.calc_spsc <- function(data_name, obj_mode, spp_cand, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (obj_mode == "XGBoost"){
    ds.calc_spsc_xgboost()
  }
}