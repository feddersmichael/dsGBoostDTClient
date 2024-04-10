
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
  
  save_list <- list(drop_columns = drop_columns)
  exist_check <- c(drop_columns = TRUE)
  ds.save_variables(data_name, save_list, exist_check, datasources)

  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }

  cally <- call("data_format_checkDS", data_name, bounds_and_levels, output_var,
                loss_function, drop_NA)
  data_classes <- DSI::datashield.aggregate(datasources, cally)
  
  reduce_classes <- function(S_1, S_2) {
    if (!identical(S_1, S_2)) {
      warning("The order of data does not coincide between the server.")
    }
    return(S_2)
  }
  data_classes <- Reduce(reduce_classes, data_classes)
  
  # We can now remove the output variable from the data_classes and the boundary
  # list
  available_columns <- names(data_classes)
  var_no <- which(output_var == available_columns)
  data_classes <- data_classes[-var_no]
  bounds_and_levels <- bounds_and_levels[-var_no]
  
  save_list <- list(output_var = output_var,
                    loss_function = loss_function,
                    data_classes = data_classes,
                    bounds_and_levels = bounds_and_levels)
  exist_check <- c(output_var = TRUE,
                   loss_function = TRUE,
                   data_classes = TRUE,
                   bounds_and_levels = TRUE)
  ds.save_variables(data_name, save_list, exist_check, datasources)

  return(list(data_classes, bounds_and_levels))
}
