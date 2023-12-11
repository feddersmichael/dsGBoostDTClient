
ds.train_boosted_tree <- function(max_treecount = 50, seed = NULL, 
                                  datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.integer(max_treecount)){
    stop("'max_treecount' needs to have data type 'integer'.")
  }
  
  if (is.null(seed)){
    set.seed()
  }
  else if (!is.integer(seed)){
    stop("'seed' needs to have data type 'integer'.")
  }
  
  
  # In this loop we train up to 'max_treecount' amount of trees.
  # If the function 'ds.train_tree' returns a break criteria instead of a tree
  # we stop the loop and return the trained boosted tree.
  
  # We initiate our list of trees
  tree_list <- list()
  
  for (i in 1:max_treecount){
    
    # We train the next tree.
    tree <- ds.train_tree(data_name, cand_select_mode)
    
    # Depending on the outcome we add the tree or end the model training.
    if (is.character(tree)){
      break
    }
    else {
      append(tree_list, list(tree))
    }
    
  }
  
  # After the training we can save our model locally.
  ds.save_boosted_tree()
  
  
  
  # We do some basic checks about the saved data
  ds.data_format_check(data_name, split_ratio, only_numeric, datasources)
  
  # Now we need to split up the data in a test and training data set.
  # In this process we also save the column names and the feature and output
  # data frames separately on the server
  ds.prepare_dataset(train_test_ratio, data_name, datasources)
  
  # We save our tree in a (amount of splits)x4 data frame.
  
  # The first two columns save the split points.
  # Each row represents one split point with the first column showing the
  # feature along which we split (5th e.g.) and the second column showing the
  # splitting condition
  
  # The last two columns denote for the right and left path either the row
  # number under which we find their splitting condition if we have further
  # splits or the weight if the tree ends.
  
  
  
  
  
  
  
  
  
  
  
  
}