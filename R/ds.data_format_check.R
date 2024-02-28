
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

  if (!output_var %in% names(bounds_and_levels)) {
    stop("'output_var' needs to be an element of 'bounds_and_levels'.")
  }

  if (any(drop_columns %in% names(bounds_and_levels))) {
    stop("The variables for which we specified bounds and levels can't be dropped.")
  }

  cally <- call("data_format_checkDS", data_name, bounds_and_levels, output_var,
                loss_function, drop_columns, drop_NA)
  output <- DSI::datashield.aggregate(datasources, cally)
}
