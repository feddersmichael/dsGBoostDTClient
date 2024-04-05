
#' Save variables
#'
#' @param data_name Name of the data.
#' @param variable_list Named list of variables which shall be saved.
#' @param datasources Datasource connections.
#'
#' @return Nothing.
#' @export
ds.save_variables <- function(data_name, variable_list, datasources){
  
  for (variable_name in names(variable_list)) {
    cally <- call("save_variableDS", data_name, variable_list[[variable_name]])
    DSI::datashield.assign.expr(datasources,
                                paste0(data_name, "_", variable_name), cally)
  }
}