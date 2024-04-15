
#' Calculate the predicted output
#'
#' @param data_name The name under which the data is saved on the server.
#' @param weight_update Through which method we choose the weights for our tree.
#' @param last_tr_tree The last tree which was trained under the boosting step.
#' @param amt_trees How many trees have been built already.
#' @param datasources DATASHIELD server connection.
#'
#' @return None.
#' @export
ds.calc_hist <- function(data_name, weight_update, last_tr_tree, amt_trees,
                         datasources = NULL) {

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                            function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }

  # We call the server to generate the new histogram values based on the
  # predicted output, updated through the last trained tree

  # If 'amt_trees' is 0 we initialise the predicted output.
  if (amt_trees == 0) {
    
    save_list <- list(weight_update = weight_update)
    exist_check <- c(weight_update = TRUE)
    ds.save_variables(data_name, save_list, exist_check, datasources)
    
    cally <- call("calc_hist_initDS", data_name)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  }
  # if we already trained a tree before we just add up the predicted value from
  # the last trained tree
  else {
    if (!is.data.frame(last_tr_tree)) {
      stop("'last_tr_tree' needs to be an object of type 'data frame'.")
    }

    cally <- call("calc_histDS", data_name, last_tr_tree, amt_trees)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  }
}