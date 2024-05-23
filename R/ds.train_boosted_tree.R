
#' Training of a Gradient Boosted Decision Tree
#'
#' @param data_name The name under which the data is saved on the server.
#' @param bounds_and_levels Bounds for numeric columns and levels for factors.
#' @param output_var The name of the column containing the output variable.
#' @param drop_columns Vector of data columns which shall be removed.
#' @param drop_NA If NA data in the output variable should be removed.
#' @param train_test_ratio Percentage of the data which should be used for
#' Training.
#' @param federation Through which method we share the data between client and
#' the servers.
#' @param max_treecount Maximum amount of trees to build our boosted decision
#' tree.
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param split_method Through which method we choose the tree-splits.
#' @param loss_function The name of the loss function we want to use for our
#' boosted tree.
#' @param amt_spp The amount of split-points per feature.
#' @param feature_subsampling Which part of the feature space we use to build
#' the trees.
#' @param cand_select Splitting-point selection for numeric and factor features.
#' @param weight_update Through which method we choose the weights for our tree.
#' @param reg_par Regularisation parameter which prevent overfitting.
#' @param shrinkage How high the newly trained tree effects the boosted tree.
#' @param dropout_rate Chance that a tree is not used for building the next
#' tree.
#' @param ithess_stop Maximum amount of times we update the split-point
#' candidates if the split-method is "totally_random".
#' @param seed If we want to choose a specific random behavior client side.
#' @param datasources DATASHIELD server connection.
#'
#' @return The trained decision tree model.
#' @export
ds.train_boosted_tree <- function(data_name, bounds_and_levels, output_var,
                                  drop_columns = NULL, drop_NA = TRUE,
                                  train_test_ratio = 0.9, federation,
                                  max_treecount = 10L,
                                  max_splits = 5L, split_method, loss_function,
                                  amt_spp, feature_subsampling = NULL,
                                  cand_select = c(numeric = "ithess",
                                                  factor = "exact"),
                                  weight_update, reg_par = c(lambda = 5,
                                                             gamma = 5),
                                  shrinkage = 0.1, dropout_rate = 0.05,
                                  ithess_stop = max_treecount, seed = NULL,
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
  
  if (!is.character(data_name) || !is.atomic(data_name)) {
    stop("'data_name' needs to be an atomic 'character' vector.")
  }
  
  if (!is.list(bounds_and_levels) || length(bounds_and_levels) < 2) {
    stop("'bounds_and_levels' needs be a list with at least two elements.")
  }
  
  if (!is.character(output_var) || !is.atomic(output_var)) {
    stop("'output_var' needs to be an atomic 'character' vector.")
  } else if (!output_var %in% names(bounds_and_levels)) {
    stop("'output_var' needs to be an element of 'bounds_and_levels'.")
  }
  
  if (!is.null(drop_columns) && !is.character(drop_columns)) {
    stop("'drop_columns' needs to be either a vector of data type 'character' or 'NULL'.")
  } else if (any(drop_columns %in% names(bounds_and_levels))) {
    stop("The variables for which we specified bounds and levels can't be dropped.")
  }
  
  if (!is.logical(drop_NA) || !is.atomic(drop_NA)) {
    stop("'drop_NA' needs to be an atomic 'logical' vector.")
  }
  
  if (!is.numeric(train_test_ratio) || !is.atomic(train_test_ratio) ||
      (train_test_ratio <= 0) || (train_test_ratio > 1)) {
    stop("'train_test_ratio' needs to be an atomic 'numeric' vector which is greater than 0 and at most 1.")
  }
  
  if (!is.integer(max_treecount) || !is.atomic(max_treecount) || max_treecount < 1.) {
    stop("'max_treecount' needs to be an atomic 'integer' vector greater than 0.")
  }
  
  if (!is.integer(max_splits) || !is.atomic(max_splits) || max_splits < 1.) {
    stop("'max_splits' needs to be an atomic 'integer' vector greater than 0.")
  }
  if (split_method == "totally_random") {
    save_list <- list(max_splits = max_splits)
    exist_check <- c(max_splits = TRUE)
    ds.save_variables(data_name, save_list, exist_check, datasources)
  }
  
  if (!is.character(split_method) || !is.atomic(split_method)) {
    stop("'split_method' needs to be an atomic 'character' vector.")
  } else if (!(split_method %in% c("histograms", "partially_random",
                                   "totally_random"))) {
    stop("This split-method is not available.")
  }
  
  if (!is.character(loss_function) || !is.atomic(loss_function)) {
    stop("'loss_function' needs to be an atomic 'character' vector.")
  } else if (!(loss_function %in% c("quadratic", "binary_cross_entropy",
                                    "binary_sigmoid"))) {
    stop("This loss-function is not available.")
  }
  
  if (!is.integer(amt_spp) || length(amt_spp) == length(bounds_and_levels)
      || any(amt_spp < 1)) {
    stop("'amt_spp' needs to be a vector of data type 'integer' with length of 'bounds_and_levels' minus one and each element greater than 0.")
  }
  
  if(!is.null(feature_subsampling)) {
    if (!is.list(feature_subsampling) || length(feature_subsampling) != 2) {
      stop("'feature_subsampling' needs to be a list with two elements.")
    } else {
      if (!is.character(feature_subsampling[["mode"]]) || !is.atomic(feature_subsampling[["mode"]])) {
        stop("'feature_subsampling$mode' needs to be an atomic 'character' vector.")
      } else if (!(feature_subsampling[["mode"]] %in% c("cyclical", "random"))) {
        stop("This feature-subsampling mode is not available.")
      }
      if (!is.numeric(feature_subsampling[["selection"]]) || !is.atomic(feature_subsampling[["selection"]]) ||
          feature_subsampling[["selection"]] <= 0) {
        stop("'feature_subsampling$selection' needs to be an atomic 'numeric' vector greater than 0.")
      }
      if (feature_subsampling[["selection"]] >= 1 && !is.integer(feature_subsampling[["selection"]])) {
        stop("'feature_subsampling$selection' needs to be an atomic 'integer' vector if it is greater than 1.")
      }
      if (feature_subsampling[["selection"]] > length(bounds_and_levels)) {
        stop("'feature_subsampling$selection' can't be greater than the amount of features.")
      }
      if (feature_subsampling[["selection"]] == length(bounds_and_levels)) {
        stop("'feature_subsampling$selection' can't be 'length(bounds_and_levels', use 'feature_subsampling == NULL' instead.")
      }
      if (feature_subsampling[["mode"]] == "cyclical" && feature_subsampling[["selection"]] < 1) {
        stop("If 'feature_subsampling$mode' is 'cyclical', 'feature_subsampling$selection' has to be at least 1.")
      }
    }
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
  
  if (!is.character(weight_update) || !is.atomic(weight_update)) {
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
  
  if (!is.numeric(reg_par) || length(reg_par) != 2 || any(reg_par < 0) ||
      !identical(names(reg_par), c("lambda", "gamma"))) {
    stop("'reg_par' needs to be a numeric vector of length 2 with positive entries 'lambda' and 'gamma'.")
  }
  
  if (!is.numeric(shrinkage) || !is.atomic(shrinkage) ||
      shrinkage <= 0 || shrinkage > 1) {
    stop("'shrinkage' needs to be an atomic 'numeric' vector which lies between 0 and 1.")
  }
  
  if (!is.numeric(dropout_rate) || !is.atomic(dropout_rate) ||
      dropout_rate < 0 || dropout_rate > 1) {
    stop("'dropout_rate' needs to be an atomic 'numeric' vector which lies between 0 and 1.")
  } else if (dropout_rate > 0) {
      if (weight_update == "average") {
        stop("If 'dropout_rate' is not 0, 'weight_update' can't be 'average'.")  
      }
      if (shrinkage != 1) {
        stop("If 'dropout_rate' is not 0, 'shrinkage' has to be 1.")
      }
      if (dropout_rate == 1 && split_method != "totally_random") {
        stop("If 'dropout_rate' is 1, 'split_method' has to be 'totally_random'.")
      }
  }
  
  if (!is.integer(ithess_stop) || !is.atomic(ithess_stop) || ithess_stop < 1 ||
      ithess_stop > max_treecount) {
    stop("'ithess_stop' needs to be an atomic 'integer' vector between 1 and 'max_treecount'.")
  }
  
  if (!is.null(seed)) {
    if (!is.integer(seed) || !is.atomic(seed)) {
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
  
  if (!identical(sort(names(amt_spp)), sort(names(bounds_and_levels)))) {
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
  
  if (federation[["mode"]] == "histograms") {
    add_par <- list(spp_cand = list())
    for (i in 1:max_treecount) {
      amt_trees <- i - 1
      if (is.null(feature_subsampling)) {
        selected_feat <- names(data_classes)
      } else {
        if (feature_subsampling[["mode"]] == "cyclical") {
          selected_feat <- names(data_classes)[((((i - 1) * feature_subsampling[["selection"]]) %% length(data_classes)) + 1):
                                                 (((i * feature_subsampling[["selection"]] - 1) %% length(data_classes)) + 1)]
        } else if (feature_subsampling[["mode"]] == "random") {
          if (feature_subsampling[["selection"]] < 1) {
            amt_feat <- stats::rbinom(1, length(data_classes), feature_subsampling[["selection"]])
            if (amt_feat == 0) {
              amt_feat <- 1L
            }
            selected_feat <- sample(names(data_classes), amt_feat)
          } else {
            selected_feat <- sample(names(data_classes), feature_subsampling[["selection"]])
          }
        }
      }
      save_list <- list(selected_feat = selected_feat)
      exist_check <- c(selected_feat = FALSE)
      ds.save_variables(data_name, save_list, exist_check, datasources)
      
      # We train the next tree.
      tree_return <- ds.train_tree(data_name, bounds_and_levels, data_classes,
                                   output_var, amt_trees, max_splits,
                                   split_method, loss_function, amt_spp,
                                   selected_feat, cand_select, weight_update,
                                   reg_par, dropout_rate, shrinkage,
                                   ithess_stop, add_par, datasources)
      if (shrinkage < 1) {
        tree_return[[1]] <- ds.add_shrinkage(tree_return[[1]], shrinkage)
      }
      if (dropout_rate > 0) {
        scale_par <- 1 / (length(tree_return[[3]]) + 1)
        tree_return[[1]] <- ds.add_shrinkage(tree_return[[1]], scale_par)
        for (j in tree_return[[3]]) {
          tree_list[[j]] <- ds.add_shrinkage(tree_list[[j]], length(tree_return[[3]]) * scale_par)
        }
      }
      tree_list[[i]] <- tree_return[[1]]
      add_par <- tree_return[[2]]
    }
  } else if (federation[["mode"]] %in% c("trees_cyclical", "trees_random")){
    #initialisation round
    trees <- ds.initialise_remote_tree(data_name, federation, weight_update,
                                       dropout_rate, shrinkage, cand_select,
                                       ithess_stop, split_method, reg_par,
                                       feature_subsampling, data_classes,
                                       amt_spp, max_splits, datasources)
    
    if (shrinkage < 1) {
      for (j in 1:length(trees)) {
        trees[[j]] <- ds.add_shrinkage(trees[[j]], shrinkage)
      }
    }
    tree_list <- trees
    if (max_treecount > 1) {
      for (i in 2:max_treecount) {
        tree_return <- ds.train_remote_tree(data_name, federation, i,
                                      length(tree_list), feature_subsampling,
                                      data_classes, dropout_rate, shrinkage,
                                      datasources)
        trees <- tree_return[[1]]
        removed_trees <- tree_return[[2]]
        
        if (shrinkage < 1) {
          for (j in 1:length(trees)) {
            trees[[j]] <- ds.add_shrinkage(trees[[j]], shrinkage)
          }
        }
        
        if (dropout_rate > 0) {
          scale_par <- 1 / (length(trees) + length(removed_trees))
          for (j in 1:length(trees)) {
            trees[[j]] <- ds.add_shrinkage(trees[[j]], scale_par)
          }
          for (j in removed_trees) {
            tree_list[[j]] <- ds.add_shrinkage(tree_list[[j]],
                                               length(removed_trees) * scale_par)
          }
        }
        tree_list <- append(tree_list, trees)
      }
    }
  }

  # After the training we can save our model locally.
  # ds.save_boosted_tree()

  return(tree_list)
}