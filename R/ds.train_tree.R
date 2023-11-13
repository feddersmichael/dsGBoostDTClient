
ds.train_tree <- function(data, max_treecount, regul_par, spp_mode, 
                          datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  if (!class(max_treecount) == "Integer"){
    stop("max_treecount needs to be an integer.")
  }
  
  # We do some basic checks about the saved data
  ds.data_format_check(data, datasources)
  
  # Now we need to split up the data in a test and training data set.
  # The training data is saved by adding the subscript '_training' to 'data'
  # and the test data is saved by adding the subscript '_test' to 'data'.
  ds.create_data_split(split_ratio)
  
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
    tree <- ds.training_step(data, spp_mode)
    append(tree_list, list(tree))
    # need break criteria
  }
}