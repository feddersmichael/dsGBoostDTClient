
#' Creating exact Splitting-points
#'
#' @param data_name The name under which the data is saved on the server.
#' @param datasources DATASHIELD server connection.
#'
#' @return The splitting point candidates.
#' @export
ds.exact_spp_cand <- function(data_name, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- call("exact_spp_candDS", data_name)
  output <- DSI::datashield.aggregate(datasources, cally)
  
  return(output)
}