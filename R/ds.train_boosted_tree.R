

#' Training of a Gradient Boosted Decision Tree
#'
#' @param data_name The name under which the data is saved on the server.
#' @param train_test_ratio Percentage of the data which should be used for 
#' Training.
#' @param split_status Defines if 'data_name' saves the full data, is already
#' split up into Training and Test data or saves only one of them.
#' @param max_treecount Maximum amount of trees to build our boosted decision
#' tree.
#' @param amt_spp The amount of split-points per feature.
#' @param seed If we want to choose a specific random behavior client side.
#' @param drop_NA If NA data in the output variable should be removed.
#' @param bounds_and_levels Bounds for numeric columns and levels for factors.
#' @param output_var The name of the column containing the output variable.
#' @param loss_function The name of the loss function we want to use for our
#' boosted tree.
#' @param drop_columns Vector of data columns which shall be removed.
#' @param datasources DATASHIELD server connection.
#'
#' @return The trained decision tree model.
#' @export
ds.train_boosted_tree <- function(data_name, train_test_ratio, split_status,
                                  max_treecount = 50, amt_spp, seed = NULL,
                                  drop_NA, bounds_and_levels, output_var,
                                  loss_function, drop_columns,
                                  datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!all(sapply(datasources, DSI:::.isDSConnection))) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.integer(max_treecount)){
    stop("'max_treecount' needs to have data type 'integer'.")
  }
  
  if (is.null(seed)){
    set.seed()
  }
  else {
    if (!is.integer(seed)){
      stop("'seed' needs to have data type 'integer'.")
    }
    else {
      set.seed(seed)
    }
  }
  
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.list(bounds_and_levels)) {
    stop("'bounds_and_levels' needs be an object of type 'list'.")
  }
  
  if (!is.character(output_var)) {
    stop("'output_var' needs to have data type 'character'.")
  }
  
  if (!is.logical(drop_NA)) {
    stop("'drop_NA' needs to have data type 'logical'.")
  }
    
  
  # We do some basic checks about the saved data
  data_classes <- ds.data_format_check(data_name, bounds_and_levels, output_var,
                                       loss_function, drop_NA, datasources)
  
  # Before we start training our model we split up the data set into a training
  # and test part.
  ds.create_data_split(data_name, data_classes, output_var, drop_columns,
                       train_test_ratio, datasources)
  
  # Now we can adapt the 
  
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
    tree <- ds.train_tree(tree_list[[length(tree_list)]], amt_spp, data_classes)
    
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