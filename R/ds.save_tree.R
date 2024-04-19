
#' Save a single tree
#'
#' @param data_name The name under which the data is saved on the server.
#' @param tree The tree which shall be saved.
#' @param tree_number How many trees have been trained.
#' @param amt_drops How many tree were dropped to train this tree.
#' @param datasources DATASHIELD server connection.
#'
#' @return None.
#' @export
ds.save_tree <- function(data_name, tree, tree_number, amt_drops, datasources) {
  
  cally <- call("save_treeDS", data_name, tree, amt_drops)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_tree_",
                                                  tree_number), cally)
}