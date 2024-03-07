
ds.update_weight <- function(data_name, current_tree, max_splits, reg_par, weight_update,
                             datasources) {
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }
  
  cally <- call("update_weightDS", data_name, current_tree, max_splits, weight_update)
  weight_list <- DSI::datashield.aggregate(datasources, cally)
  
  amt_weights <- 2^max_splits
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
    tree_weights <- lapply(tree_weights, function(weight)
      {return(tree_weights[["output_sum"]] / tree_weights[["amt_data"]])})
  } else if (weight_update == "hessian") {
    reduce_hessian <- function(S_1, S_2) {
      for (i in 1:amt_weights) {
        S_1[[i]][["gradient"]] <- S_1[[i]][["gradient"]] + S_2[[i]][["gradient"]]
        S_1[[i]][["hessian"]] <- S_1[[i]][["hessian"]] + S_2[[i]][["hessian"]]
      }
      
      return(S_1)
    }
    
    tree_weights <- Reduce(reduce_hessian, weight_list)
    tree_weights <- lapply(tree_weights, function(weight)
      {return(weight[["gradient"]] / (weight[["hessian"]] + reg_par[[1]]))})
    
  }
  
  return(tree_weights)
}