
#' Update the trees which got changed through DART.
#'
#' @param data_name The name under which the data is saved on the server.
#' @param removed_trees Which trees got removed for training.
#' @param last_tree_nmb Number of the last trained tree.
#' @param datasources DATASHIELD server connection.
#'
#' @return None.
#' @export
ds.update_trees <- function(data_name, removed_trees, last_tree_nmb,
                            datasources) {
  
  cally <- call("update_full_treeDS", data_name, removed_trees, last_tree_nmb)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"),
                              cally)
  
  amt_drops <- length(removed_trees)
  for (i in removed_trees) {
    cally <- call("update_treeDS", data_name, i, amt_drops)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_tree_", i),
                                cally)
  }
}