
#' Save variables
#'
#' @param data_name Name of the data.
#' @param variable_list Named list of variables which shall be saved.
#' @param exist_check If that variable should be checked for existance.
#' @param datasources Datasource connections.
#'
#' @return Nothing.
#' @export
ds.save_variables <- function(data_name, variable_list, exist_check,
                              datasources){
  if (!is.logical(exist_check) ||
      (length(exist_check) != length(variable_list))) {
    stop("'exist_check' needs to be a logical vector with the same length as 'variable_list'.")
  }
  
  for (variable_name in names(variable_list)) {
    if (exist_check[[variable_name]]) {
      cally <- call("save_variableDS", data_name, variable_name,
                    variable_list[[variable_name]])
    } else {
      cally <- call("save_variableDS", data_name, NULL,
                    variable_list[[variable_name]])
    }
    DSI::datashield.assign.expr(datasources,
                                paste0(data_name, "_", variable_name), cally)
  }
}