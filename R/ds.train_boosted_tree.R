

#' Training of a Gradient Boosted Decision Tree
#'
#' @param data_name The name under which the data is saved on the server.
#' @param bounds_and_levels Bounds for numeric columns and levels for factors.
#' @param output_var The name of the column containing the output variable.
#' @param loss_function The name of the loss function we want to use for our
#' boosted tree.
#' @param train_test_ratio Percentage of the data which should be used for
#' Training.
#' @param amt_spp The amount of split-points per feature.
#' @param cand_select Splitting-point selection for numeric and factor features.
#' @param drop_columns Vector of data columns which shall be removed.
#' @param drop_NA If NA data in the output variable should be removed.
#' @param reg_par Regularisation parameter which prevent overfitting.
#' @param max_treecount Maximum amount of trees to build our boosted decision
#' tree.
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param seed If we want to choose a specific random behavior client side.
#' @param datasources DATASHIELD server connection.
#'
#' @return The trained decision tree model.
#' @export
ds.train_boosted_tree <- function(data_name, bounds_and_levels, output_var,
                                  loss_function, train_test_ratio, amt_spp,
                                  cand_select, drop_columns = NULL,
                                  drop_NA = TRUE, reg_par = c(5, 5),
                                  max_treecount = 10, max_splits = 5,
                                  seed = NULL, datasources = NULL) {

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  else if (!all(sapply(datasources, DSI:::.isDSConnection))) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.character(data_name) || !identical(length(data_name), 1)) {
    stop("'data_name' needs to be an atomic 'character' vector.")
  }
  
  if (!is.list(bounds_and_levels || length(bounds_and_levels) <= 1)) {
    stop("'bounds_and_levels' needs be a list with at least two elements.")
  }
  
  if (!is.character(output_var) || !identical(length(output_var), 1)) {
    stop("'output_var' needs to be an atomic 'character' vector.")
  }
  
  if (!is.character(loss_function) || !identical(length(loss_function), 1)) {
    stop("'loss_function' needs to be an atomic 'character' vector.")
  }
  
  if (!is.numeric(train_test_ratio) || length(train_test_ratio) != 1 ||
      (train_test_ratio < 0) || (train_test_ratio > 1)) {
    stop("'train_test_ratio' needs to be an atomic 'numeric' vector which lies between 0 and 1.")
  }
  
  if (!is.integer(amt_spp) || any(amt_spp < 1)) {
    stop("'amt_spp' needs to be a vector of data type 'integer' with each element greater than 0.")
  }
  
  if (!is.character(cand_select) || length(cand_select) != 2 ||
      !identical(names(cand_select), c("numeric", "factor"))) {
    stop("'cand_select' needs to be a vector of data type 'character' with length 2 and named elements 'numeric' and 'factor'.")
  }
  
  if (!is.null(drop_columns) && !is.character(drop_columns)) {
    stop("'drop_columns' needs to be a vector of data type 'character'.")
  }
  
  if (!is.logical(drop_NA) || length(drop_NA) != 1) {
    stop("'drop_NA' needs to be an atomic 'logical' vector.")
  }
  
  if (!is.integer(max_treecount) || length(max_treecount) != 1) {
    stop("'max_treecount' needs to be an atomic 'integer' vector.")
  }
  
  if (!is.integer(max_splits) || length(max_splits) != 1) {
    stop("'max_splits' needs to be an atomic 'integer' vector.")
  }

  if (!is.null(seed)) {
    if (!is.integer(seed) || length(seed) != 1) {
      stop("'seed' needs to be an atomic 'integer' vector.")
    }
    else {
      set.seed(seed)
    }
  }
  
  # We do some basic checks about the saved data
  data_classes <- ds.data_format_check(data_name, bounds_and_levels, output_var,
                                       loss_function, drop_NA, drop_columns,
                                       datasources)
  
  # Before we start training our model we split up the data set into a training
  # and test part.
  ds.create_data_split(data_name, output_var, drop_columns, train_test_ratio,
                       datasources)
  
  # We can now remove the output variable from the data_classes and the boundary.
  # list
  available_columns <- names(data_classes)
  var_no <- which(output_var == available_columns)[1]
  data_classes <- data_classes[-var_no]
  bounds_and_levels <- bounds_and_levels[-var_no]
  
  # We initiate our list of trees with 'NULL' which symbolizes an empty tree
  tree_list <- list(NULL)
  
  # In this loop we train up to 'max_treecount' amount of trees.
  # If the function 'ds.train_tree' returns a break criteria instead of a tree
  # we stop the loop and return the trained boosted tree.
  
  for (i in 1:max_treecount){
    
    last_tr_tree <- tree_list[[length(tree_list)]]
    
    # We train the next tree.
    tree <- ds.train_tree(data_name, last_tr_tree, bounds_and_levels,
                          data_classes, output_var, loss_function, amt_spp,
                          cand_select, reg_par, max_splits, datasources)
    
    # Depending on the outcome we add the tree or end the model training.
    if (is.character(tree)){
      break
    }
    else {
      tree_list[[i]] <- tree
    }
  }
  
  # After the training we can save our model locally.
  # ds.save_boosted_tree()
  
  return(tree_list)
}