
#' Calculate the predicted output
#'
#' @param data_name The name under which the data is saved on the server.
#' @param last_tr_tree The last tree which was trained under the boosting step.
#' @param loss_function The loss function under which we optimize our boosted
#' tree building process.
#' @param output_var The name of the column containing the output.
#' @param datasources DATASHIELD server connection.
#'
#' @return None.
#' @export
ds.calc_hist <- function(data_name, last_tr_tree, loss_function, output_var,
                         datasources = NULL) {

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  else if (!all(sapply(datasources, DSI:::.isDSConnection))) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }

  if (!is.character(data_name)) {
    stop("'data_name' needs to have data type 'character'.")
  }

  if (!is.character(loss_function)) {
    stop("'loss_function' needs to have data type 'character'.")
  }
  # We call the server to generate the new histogram values based on the
  # predicted output, updated through the last trained tree

  # If 'last_tr_tree' is NULL we initialise the predicted output with 0's.
  if (is.null(last_tr_tree)) {
    cally <- call("calc_hist_initDS", data_name, loss_function, output_var)
    output <- DSI::datashield.assign.expr(datasources,
                                          paste0(data_name, "_training"), cally)
  }
  # if we already trained a tree before we just add up the predicted value from
  # the last trained tree
  else {
    if (!is.data.frame(last_tr_tree)) {
      stop("'last_tr_tree' needs to be an object of type 'data frame'.")
    }

    cally <- call("calc_histDS", data_name, last_tr_tree, loss_function)
    output <- DSI::datashield.assign.expr(datasources,
                                          paste0(data_name, "_training"), cally)
  }

}