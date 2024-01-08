
#' ds.exact_spp_cand
#'
#' @param data_name 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.exact_spp_cand <- function(data_name, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  cally <- call("exact_spp_candDS", data_name)
  output <- DSI::datashield.aggregate(datasources, cally)
  
  return(output)
}