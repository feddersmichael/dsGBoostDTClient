
#' Calculate the predicted output
#'
#' @param data_name The name under which the data is saved on the server.
#' @param weight_update Through which method we choose the weights for our tree.
#' @param amt_trees How many trees have been built already.
#' @param dropout_rate Chance that a tree is not used for building the next
#' tree.
#' @param datasources DATASHIELD server connection.
#'
#' @return WHich trees got removed from training.
#' @export
ds.calc_hist <- function(data_name, weight_update, amt_trees, dropout_rate,
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

  removed_trees <- c()
  # We call the server to generate the new histogram values based on the
  # predicted output, updated through the last trained tree

  # If 'amt_trees' is 0 we initialise the predicted output.
  if (amt_trees == 0) {
    
    save_list <- list(weight_update = weight_update,
                      dropout_rate = dropout_rate)
    exist_check <- c(weight_update = TRUE, dropout_rate = TRUE)
    ds.save_variables(data_name, save_list, exist_check, datasources)
    
    cally <- call("calc_hist_initDS", data_name)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
    
    cally <- call("update_full_treeDS", data_name, NULL, 0)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_full_tree"),
                                cally)
  }
  # if we already trained a tree before we just add up the predicted value from
  # the last trained tree
  else {
    if (0 < dropout_rate && dropout_rate < 1) {
      amt_removed_trees <- stats::rbinom(1, amt_trees, dropout_rate)
      if (amt_removed_trees == 0) {
        amt_removed_trees <- 1
      }
      removed_trees <- sample.int(amt_trees, amt_removed_trees)
    }
    
    cally <- call("calc_histDS", data_name, amt_trees, removed_trees)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  }
  
  return(removed_trees)
}