
ds.train_tree <- function(data, max_treecount, regul_par, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (!class(max_treecount) == "Integer"){
    stop("max_treecount needs to be an integer.")
  }
  
  # need to clarify if data is already split up
  ds.data_format_check(data, datasources)
  
  
  
  # We save our tree in a nx4 matrix.
  
  # The first two columns save the split points.
  # Each row represents one split point with the first column showing the
  # feature along which we split (5th e.g.) and the second column showing the
  # splitting condition
  
  # The last two columns denote for the right and left path either the row
  # number under which we find their splitting condition if we have further
  # splits or the weight if the tree ends.
  # tree <- matrix(nrow = 0, ncol = 4)
  tree_list <- list()
  
  for (i in 1:max_treecount){
    tree <- ds.training_step(data)
    append(tree_list, list(tree))
    # need break criteria
  }
}