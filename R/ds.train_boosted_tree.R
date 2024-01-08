
#' ds.train_boosted_tree
#'
#' @param max_treecount 
#' @param seed 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
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
  
  # We do some basic checks about the saved data
  ds.data_format_check(data_name, split_ratio, only_numeric, datasources)
  
  
  # Before we start training our model we split up the data set into a training
  # and test part.
  ds.create_data_split(data_name, train_test_ratio, split_status, datsources)
  
  
  # We save our tree in a (amount of splits)x8 data frame. Each row represents
  # one split point.
  
  # Column 1 denotes the feature along which we split (5th e.g.).
  # Column 2 then denotes the exact splitting value along we split the data.
  
  # Columns 3 is 'TRUE' if the left leave is a 'weight' and 'FALSE' if the left
  # leave is a 'weight'.
  # Column 4 denotes either the row-number of the split-point or the weight at
  # the left leaf.
  
  # Columns 5 is 'TRUE' if the right leave is a 'weight' and 'FALSE' if the
  # right leave is a 'weight'.
  # Column 6 denotes either the row-number of the split-point or the weight at
  # the right leaf.
  
  # Columns 5 shows whether the right leave is a 'split' or a 'weight' and
  # column 6 has either the row-number of the split-point or the weight at the 
  # right leaf
  
  # Column 7 identifies the row of the parent split point.
  # Column 8 is 'TRUE' if we reach the parent node from the 'left' or 'FALSE' if
  # we reach the parent node from the 'right' branch.
  
  # We initiate our list of trees with 'NULL' which symbolizes an empty tree
  tree_list <- list(NULL)
  
  # In this loop we train up to 'max_treecount' amount of trees.
  # If the function 'ds.train_tree' returns a break criteria instead of a tree
  # we stop the loop and return the trained boosted tree.
  
  for (i in 1:max_treecount){
    
    # We train the next tree.
    tree <- ds.train_tree(treelist[[length(tree_list)]])
    
    # Depending on the outcome we add the tree or end the model training.
    if (is.character(tree)){
      break
    }
    else {
      tree_list <- append(tree_list, tree)
    }
  }
  
  # After the training we can save our model locally.
  # ds.save_boosted_tree()
  
  return(tree_list)
}