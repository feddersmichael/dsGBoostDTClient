
#' Update weights
#'
#' @param data_name The name under which the data is saved on the server.
#' @param current_tree The fully chosen tree which misses its weights.
#' @param bounds_and_levels The maximum and minimum values for numeric features
#' and levels for factor features. 
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param data_classes List of data class per column.
#' @param reg_par Regularisation parameter which prevent overfitting.
#' @param weight_update Through which method we choose the weights for our tree.
#' @param loss_function The type of loss function under which we optimise the
#' tree.
#' @param output_var Name of the output variable. 
#' @param datasources DATASHIELD server connection.
#'
#' @return The optimal weights for the current tree.
#' @export
ds.update_weight <- function(data_name, current_tree, bounds_and_levels,
                             max_splits, data_classes, reg_par, weight_update,
                             loss_function, output_var, datasources) {
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }
  cally <- call("update_weightDS", data_name, current_tree, bounds_and_levels,
                max_splits, data_classes, weight_update, loss_function,
                output_var)
  weight_list <- DSI::datashield.aggregate(datasources, cally)
  amt_weights <- length(weight_list[[1]])
  tree_weights <- numeric()
  if (weight_update == "average") {
    reduce_average <- function(S_1, S_2) {
      for (i in 1:amt_weights) {
        S_1[[i]][["output_sum"]] <- S_1[[i]][["output_sum"]] + S_2[[i]][["output_sum"]]
        S_1[[i]][["amt_data"]] <- S_1[[i]][["amt_data"]] + S_2[[i]][["amt_data"]]
      }
      return(S_1)
    }
    tree_weights <- Reduce(reduce_average, weight_list)
    
    calculate_weight <- function(weight) {
      if (weight[["amt_data"]] == 0) {
        return(0)
      } else {
        return(weight[["output_sum"]] / weight[["amt_data"]])
      }
    }
    
    tree_weights <- lapply(tree_weights, calculate_weight)
  } else if (weight_update == "hessian") {
    reduce_hessian <- function(S_1, S_2) {
      for (i in 1:amt_weights) {
        S_1[[i]][["gradient"]] <- S_1[[i]][["gradient"]] + S_2[[i]][["gradient"]]
        S_1[[i]][["hessian"]] <- S_1[[i]][["hessian"]] + S_2[[i]][["hessian"]]
      }
      
      return(S_1)
    }
    tree_weights <- Reduce(reduce_hessian, weight_list)
    
    calculate_weight <- function(weight) {
      if (weight[["hessian"]] + reg_par[[1]] == 0) {
        return(0)
      } else {
        return(-weight[["gradient"]] / (weight[["hessian"]] + reg_par[[1]]))
      }
    }
    
    tree_weights <- lapply(tree_weights, calculate_weight)
  }
  
  return(tree_weights)
}