
ds.train_remote_tree <- function(selected_server, datasources) {
  
  if (dropout_rate < 1) {
    if (dropout_rate > 0) {
      amt_removed_trees <- stats::rbinom(1, amt_trees, dropout_rate)
      if (amt_removed_trees == 0) {
        amt_removed_trees <- 1
      }
      removed_trees <- sample.int(amt_trees, amt_removed_trees)
    }
    
    cally <- call("calc_histDS", data_name, amt_trees, removed_trees)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  } else {
    removed_trees <- 1:amt_trees
  }
  
  cally <- call("train_treeDS", data_name)
  trees <- DSI::datashield.aggregate(datasources, cally)
  
  
  return(trees)
}