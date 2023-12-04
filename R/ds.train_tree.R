
ds.train_tree <- function(data_name, max_treecount, regul_par, cand_select_mode, 
                          datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (!class(max_treecount) == "Integer"){
    stop("max_treecount needs to be an integer.")
  }
  
  # We do some basic checks about the saved data
  ds.data_format_check(data_name, split_ratio, only_numeric, datasources)
  
  # Now we need to split up the data in a test and training data set.
  # In this process we also save the column names and the feature and output
  # data frames separately on the server
  ds.prepare_dataset(train_test_ratio, data_name, datasources)
  
  # We save our tree in a (amount of splits)x4 matrix.
  
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
    tree <- ds.training_step(data_name, cand_select_mode)
    append(tree_list, list(tree))
    # need additional break criteria
  }
  
  ds.save_tree()
  
}