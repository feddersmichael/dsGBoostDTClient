
ds.exact_spp_cand <- function(data, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- call("exact_spp_candDS", data)
  output <- DSI::datashield.aggregate(datasources, cally)
  
  return(output)
}