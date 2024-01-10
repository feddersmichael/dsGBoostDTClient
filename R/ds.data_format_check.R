
#' Check Data for basic rules
#'
#' @param data_name The name under which the data is saved on the server.
#' @param datasources DATASHIELD server connection.
#'
#' @return NONE.
#' @export
ds.data_format_check <- function(data_name, bounds_and_levels, drop_NA = FALSE,
                                 datasources = NULL) {
  # We want to check in a generic way if the uploaded data fulfills
  # our requirements to be used in this analysis.
  # TODO: dimension check between servers

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  else if (!all(sapply(datasources, DSI:::.isDSConnection))) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }

  # We check if 'data_name' is of type character.
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.logical(drop_NA)) {
    stop("'drop_NA' needs to have data type 'logical'.")
  }
  
  if (!is.list(bounds_and_levels)) {
    stop("'bounds_and_levels' needs be an object of type 'list'.")
  }

  # We start by checking if a data frame with name 'data_name' exists on all
  # servers and their column names coincide.
  
  cally <- call("data_format_checkDS", data_name, col_names, drop_NA)
  result <- DSI::datashield.aggregate(datasources, cally)

}
