
#' Check Data for basic rules
#'
#' @param data_name The name under which the data is saved on the server
#' @param bounds_and_levels List of all columns which stores either bounds or
#' level sets for all variables.
#' @param output_var The name of the output variable.
#' @param loss_function The loss function which we use to optimize our boosted
#' tree.
#' @param drop_columns Remove unnecessary columns from the data_check
#' @param drop_NA If NA-values should be deleted.
#' @param datasources DATASHIELD server connection.
#'
#' @return The data classes (in the sense of data.class()) for all columns.
#' @export
ds.data_format_check <- function(data_name, bounds_and_levels, output_var,
                                 loss_function, drop_columns = NULL,
                                 drop_NA = TRUE, datasources = NULL) {
  # We want to check in a generic way if the uploaded data fulfills
  # our requirements to be used in this analysis.

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }

  cally <- call("data_format_checkDS", data_name, bounds_and_levels, output_var,
                loss_function, drop_columns, drop_NA)
  data_classes <- DSI::datashield.aggregate(datasources, cally)
  
  # We can now remove the output variable from the data_classes and the boundary.
  # list
  available_columns <- names(data_classes[[1]])
  var_no <- which(output_var == available_columns)[1]
  data_classes <- data_classes[[1]][-var_no]
  bounds_and_levels <- bounds_and_levels[-var_no]

  return(list(data_classes, bounds_and_levels))
}
