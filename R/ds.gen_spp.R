

# Generate splitting points
ds.gen_spp <- function(data, mode = "percentile", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (mode == "exact"){
    spp <- exact_sppDS(data)
  }
  else if (mode == "percentile"){
    spp <- percentile_sppDS(data)
  }
  else {
    stop("'mode' is not one of the available options.")
  }
  
  return(spp)
}