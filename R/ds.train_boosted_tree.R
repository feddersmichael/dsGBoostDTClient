

#' Training of a Gradient Boosted Decision Tree
#'
#' @param data_name The name under which the data is saved on the server.
#' @param split_method Through which method we choose the tree-splits.
#' @param weight_update Through which method we choose the weights for our tree.
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
#' @param ithess_stop Maximum amount of times we update the split-point
#' candidates if the split-method is "totally_random"
#' @param shrinkage How high the newly trained tree effects the boosted tree.
#' @param max_treecount Maximum amount of trees to build our boosted decision
#' tree.
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param seed If we want to choose a specific random behavior client side.
#' @param datasources DATASHIELD server connection.
#'
#' @return The trained decision tree model.
#' @export
ds.train_boosted_tree <- function(data_name, split_method, weight_update,
                                  bounds_and_levels, output_var, loss_function,
                                  train_test_ratio, amt_spp, cand_select,
                                  drop_columns = NULL, drop_NA = TRUE,
                                  reg_par = c(5, 5), ithess_stop = NULL,
                                  shrinkage = 0.1, max_treecount = 10,
                                  max_splits = 5, seed = NULL,
                                  datasources = NULL) {

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }

  if (!is.character(split_method) || length(split_method) != 1) {
    stop("'split_method' needs to be an atomic 'character' vector.")
  } else if (!(split_method %in% c("histograms", "partially_random",
                                   "totally_random"))) {
    stop("This split-method is not available.")
  }
  
  if (!is.character(weight_update) || length(weight_update) != 1) {
    stop("'weight_update' needs to be an atomic 'character' vector.")
  } else {
    if (split_method == "totally_random") {
      if (!(weight_update %in% c("hessian", "average"))) {
        stop("This weight-update is not available.")
      }
    } else {
      if (weight_update != "hessian") {
        stop("The weight-update has to be 'hessian'")
      }
    }
  }
  
  if (!is.character(data_name) || length(data_name) != 1) {
    stop("'data_name' needs to be an atomic 'character' vector.")
  }

  if (!is.list(bounds_and_levels) || length(bounds_and_levels) <= 1) {
    stop("'bounds_and_levels' needs be a list with at least two elements.")
  }

  if (!is.character(output_var) || length(output_var) != 1) {
    stop("'output_var' needs to be an atomic 'character' vector.")
  } else if (!output_var %in% names(bounds_and_levels)) {
    stop("'output_var' needs to be an element of 'bounds_and_levels'.")
  }

  if (!is.character(loss_function) || length(loss_function) != 1) {
    stop("'loss_function' needs to be an atomic 'character' vector.")
  } else if (!(loss_function %in% c("quadratic", "binary_cross_entropy",
                                    "binary_sigmoid"))) {
    stop("This loss-function is not available.")
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
  } else if (!cand_select[["numeric"]] %in% c("uniform", "loguniform",
                                              "uniform_rand", "ithess")) {
    stop(paste0("The mode '", cand_select[["numeric"]],
                "' is not supported to create split points for numeric features."))
  } else if (!cand_select[["factor"]] %in% c("exact")) {
    stop(paste0("The mode '", cand_select[["factor"]],
                "' is not supported to create split points for factor features."))
  }

  if (!is.null(drop_columns) && !is.character(drop_columns)) {
    stop("'drop_columns' needs to be a vector of data type 'character'.")
  } else if (any(drop_columns %in% names(bounds_and_levels))) {
    stop("The variables for which we specified bounds and levels can't be dropped.")
  }

  if (!is.logical(drop_NA) || length(drop_NA) != 1) {
    stop("'drop_NA' needs to be an atomic 'logical' vector.")
  }
  
  if (!is.numeric(reg_par) || length(reg_par) != 2 || any(reg_par < 0)) {
    stop("'reg_par' needs to be a numeric vector of length 2 with positive entries.")
  }
  
  if (!is.integer(ithess_stop) || length(ithess_stop) != 1 || ithess_stop < 0) {
    stop("'ithess_stop' needs to be an atomic 'integer'vector greater or equal than 0.")
  }
  
  if (!is.numeric(shrinkage) || length(shrinkage) != 1 ||
      (shrinkage <= 0) || (shrinkage > 1)) {
    stop("'shrinkage' needs to be an atomic 'numeric' vector which lies between 0 and 1.")
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
    } else {
      set.seed(seed)
    }
  }

  # We do some basic checks about the saved data
  data_checks <- ds.data_format_check(data_name, bounds_and_levels, output_var,
                                       loss_function, drop_columns, drop_NA,
                                       datasources)
  
  data_classes <- data_checks[[1]]
  bounds_and_levels <- data_checks[[2]]
  
  if (!identical(names(amt_spp), names(bounds_and_levels))) {
    stop("The features in 'bounds_and_levels' and 'amt_spp' don't coincide.")
  }

  # Before we start training our model we split up the data set into a training
  # and test part.
  ds.create_data_split(data_name, train_test_ratio, datasources)

  # We initiate our list of trees with 'NULL' which symbolizes an empty tree
  tree_list <- list()

  # In this loop we train up to 'max_treecount' amount of trees.
  # If the function 'ds.train_tree' returns a break criteria instead of a tree
  # we stop the loop and return the trained boosted tree.
  add_par <- NULL
  for (i in 1:max_treecount) {
    
    amt_trees <- i - 1
    if (amt_trees == 0) {
      last_tr_tree <- NULL
    } else {
      last_tr_tree <- tree_list[[length(tree_list)]]
    }
    
    # We train the next tree.
    tree_return <- ds.train_tree(data_name, split_method, weight_update,
                                 last_tr_tree, bounds_and_levels, data_classes,
                                 output_var, loss_function, amt_spp,
                                 cand_select, reg_par, max_splits, add_par,
                                 amt_trees, ithess_stop, datasources)
    
    if (weight_update == "hessian") {
      tree_list[[i]] <- ds.add_shrinkage(tree_return[[1]], shrinkage)
    } else if (weight_update == "average") {
      tree_list[[i]] <- tree_return[[1]]
    }
    
    add_par <- tree_return[[2]]
  }

  # After the training we can save our model locally.
  # ds.save_boosted_tree()

  return(tree_list)
}