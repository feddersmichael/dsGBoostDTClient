

# Generate splitting points
ds.gen_spp <- function(data, mode = "percentile", datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (mode == "exact"){
    spp <- ds.exact_spp(data)
  }
  else if (mode == "percentile"){
    spp <- ds.percentile_spp(data)
  }
  else {
    stop("'mode' is not one of the available options.")
  }
  
  return(spp)
}