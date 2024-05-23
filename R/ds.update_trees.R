
#' Update the trees which got changed through DART.
#'
#' @param data_name The name under which the data is saved on the server.
#' @param removed_trees Which trees got removed for training.
#' @param added_trees Numbers of the trees added this round.
#' @param datasources DATASHIELD server connection.
#'
#' @return None.
#' @export
ds.update_trees <- function(data_name, removed_trees, added_trees,
                            datasources) {
  
  cally <- call("update_full_treeDS", data_name, removed_trees, added_trees)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"),
                              cally)
  
  amt_drops <- length(removed_trees)
  amt_adds <- length(added_trees)
  for (i in removed_trees) {
    cally <- call("update_treeDS", data_name, i, amt_drops, amt_adds)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_tree_", i),
                                cally)
  }
}