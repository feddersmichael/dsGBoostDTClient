
#' Check Data for basic rules
#'
#' @param data_name The name under which the data is saved on the server.
#' @param datasources DATASHIELD server connection.
#'
#' @return NONE.
#' @export
ds.data_format_check <- function(data_name, datasources = NULL) {
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
    stop("'data_name' needs to be a character with the name of the data file.")
  }

  # Now we check if the file with this name exists and is a data frame.
  if (!all("data.frame" %in%  dsBaseClient::ds.class(data_name, datasources))) {
    stop(paste0("The data needs to be saved as a data frame object instead of ",
                dsBaseClient::ds.class(data_name, datasources)))
  }

}
