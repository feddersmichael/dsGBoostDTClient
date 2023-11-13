
ds.training_step <- function(data, spp_mode, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  ds.gen_spp(data, spp_mode, datasources)
  
  ssc <- 1
  
  while (TRUE) {
    ds.gen_spp(data, spp_mode, datasources)
    
    
  }
}