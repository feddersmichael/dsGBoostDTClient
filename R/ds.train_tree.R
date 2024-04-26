
#' Train a single tree
#'
#' @param data_name The name under which the data is saved on the server.
#' @param bounds_and_levels The maximum and minimum values for numeric features
#' and levels for factor features.
#' @param data_classes Data class for all features.
#' @param output_var Name of the output variable.
#' @param amt_trees How many trees have been built already.
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param split_method Through which method we choose the tree-splits.
#' @param loss_function The type of loss function under which we optimise the
#' tree.
#' @param amt_spp The amount of splitting point candidates per feature.
#' @param cand_select Splitting-point selection for numeric and factor features.
#' @param weight_update Through which method we choose the weights for our tree.
#' @param reg_par Regularisation parameter which prevent overfitting.
#' @param dropout_rate Chance that a tree is not used for building the next
#' tree.
#' @param ithess_stop Maximum amount of times we update the split-point
#' candidates if the split-method is "totally_random"
#' @param add_par Additional parameters for the iterative hessian mode.
#' @param datasources DATASHIELD server connection.
#'
#' @return The trained tree.
#' @export
ds.train_tree <- function(data_name, bounds_and_levels, data_classes,
                          output_var, amt_trees, max_splits = 5L, split_method,
                          loss_function, amt_spp, cand_select,  weight_update,
                          reg_par = c(lambda = 5, gamma = 5),
                          dropout_rate = 0.05, ithess_stop, add_par = NULL,
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

  # We first update the histogram values, which are based on the previously
  # trained trees.
  removed_trees <- ds.calc_hist(data_name, weight_update, amt_trees,
                                dropout_rate, datasources)
    
  if (cand_select[["numeric"]] == "ithess") {
    if (amt_trees == 0) {
      # TODO: Possibility to combine uniform and ithess in first round
      spp_cand <- ds.gen_spp_cand(data_name, bounds_and_levels, data_classes,
                                  amt_spp, list(numeric = "uniform",
                                                factor = cand_select[["factor"]]),
                                  add_par, TRUE, split_method,
                                  datasources)
    } else {
      spp_cand <- ds.gen_spp_cand(data_name, bounds_and_levels, data_classes,
                                  amt_spp, cand_select, add_par,
                                  amt_trees <= ithess_stop, split_method,
                                  datasources)
    }
    add_par[["spp_cand"]] <- spp_cand
  } else if (cand_select[["numeric"]] %in% c("uniform", "loguniform")){
    if (amt_trees == 0) {
      spp_cand <- ds.gen_spp_cand(data_name, bounds_and_levels, data_classes,
                                  amt_spp, cand_select, add_par, TRUE, NULL,
                                  datasources)
    } else {
      spp_cand <- ds.gen_spp_cand(data_name, bounds_and_levels, data_classes,
                                  amt_spp, cand_select, add_par, FALSE, NULL,
                                  datasources)
    }
    add_par[["spp_cand"]] <- spp_cand
  } else {
    spp_cand <- ds.gen_spp_cand(data_name, bounds_and_levels, data_classes,
                                amt_spp, cand_select, add_par, TRUE, NULL,
                                datasources)
  }
  
  # We save our tree in a (amount of splits)x8 data frame. Each row represents
  # one split point.

  # Column 1 denotes the feature along which we split by name as a character.
  # Column 2 then denotes the exact splitting value along we split the data.
  # Column 3 denotes for numeric features if NA-values should be considered as
  # lower or higher than the split values. "1" represents lower, "2" higher and
  # 0 that there is no bias.

  # Column 4 is 'TRUE' if the left leave is a 'weight' and 'FALSE' if the left
  # leave is a 'weight'.
  # Column 5 denotes either the row-number of the split-point or the weight at
  # the left leaf.

  # Column 6 is 'TRUE' if the right leave is a 'weight' and 'FALSE' if the
  # right leave is a 'weight'.
  # Column 7 denotes either the row-number of the split-point or the weight at
  # the right leaf.

  # Column 8 identifies the row of the parent split point.
  # Column 9 is 'TRUE' if we reach the parent node from the 'left' or 'FALSE' if
  # we reach the parent node from the 'right' branch.

  current_tree <- data.frame(feature = character(), split_value = numeric(),
                             cont_NA = numeric(), w_s_left = logical(),
                             w_s_left_value = numeric(), w_s_right = logical(),
                             w_s_right_value = numeric(), par_spp = numeric(),
                             par_dir = logical())
  
  # In this loop we build a tree with up to 'max_splits' many splits.
  for (i in 1:max_splits) {
    if (split_method == "histograms") {
      
      histograms_per_leave <- ds.split_bins(data_name, current_tree,
                                            data_classes, datasources)
      
      # We search for the best possible split(s) in the newly added branch.
      best_split <- ds.select_split(histograms_per_leave, spp_cand, data_classes,
                                    reg_par)
      
      if (i == 1) {
        split_scores_left <- data.frame(sp_sc = numeric(), feature = character(),
                                        split_val = numeric(), cont_NA = numeric(),
                                        weight_l = numeric(), weight_r = numeric())
        
        split_scores_right <- data.frame(sp_sc = numeric(), feature = character(),
                                         split_val = numeric(), cont_NA = numeric(),
                                         weight_l = numeric(), weight_r = numeric())
        
        next_split <- list(best_split$feature[[1]], best_split$split_val[[1]],
                            best_split$cont_NA[[1]], TRUE, best_split$weight_l[[1]],
                            TRUE, best_split$weight_r[[1]], 0, TRUE)
        current_tree[1, ] <- next_split
        if (cand_select[["numeric"]] == "ithess") {
          add_par[["hessians"]] <- histograms_per_leave[[1]]$hess
        }
      } else {
        # TODO: Fix rownames for copying rows into df
        split_scores_left[i - 1, ] <- best_split[1, ]
        split_scores_right[i - 1, ] <- best_split[2, ]
        
        max_l_index <- which.max(split_scores_left$sp_sc)
        max_r_index <- which.max(split_scores_right$sp_sc)
        
        max_l <- split_scores_left$sp_sc[[max_l_index]]
        max_r <- split_scores_right$sp_sc[[max_r_index]]
        
        if (max_l > 0 || max_r > 0) {
          if (max_l > max_r) {
            next_split <- split_scores_left[max_l_index, ]
            current_tree[i, ] <- list(next_split$feature[[1]],
                                      next_split$split_val[[1]],
                                      next_split$cont_NA[[1]], TRUE,
                                      next_split$weight_l[[1]], TRUE,
                                      next_split$weight_r[[1]], max_l_index,
                                      TRUE)
            current_tree$w_s_left[[max_l_index]] <- FALSE
            current_tree$w_s_left_value[[max_l_index]] <- i
            split_scores_left$sp_sc[[max_l_index]] <- 0
          } else {
            next_split <- split_scores_right[max_r_index, ]
            current_tree[i, ] <- list(next_split$feature[[1]],
                                      next_split$split_val[[1]],
                                      next_split$cont_NA[[1]], TRUE,
                                      next_split$weight_l[[1]], TRUE,
                                      next_split$weight_r[[1]], max_r_index,
                                      FALSE)
            current_tree$w_s_right[[max_r_index]] <- FALSE
            current_tree$w_s_right_value[[max_r_index]] <- i
            split_scores_right$sp_sc[[max_r_index]] <- 0
          }
        } else {
          break
        }
      }
    } else if (split_method == "partially_random") {
      split_val <- c()
      for (feature in names(bounds_and_levels)) {
        split_val <- c(split_val, sample(spp_cand[[feature]], 1))
      }
      names(split_val) <- names(bounds_and_levels)
      # TODO: fix workaround
      save_list <- list(spp_cand = split_val)
      exist_check <- c(spp_cand = FALSE)
      ds.save_variables(data_name, save_list, datasources)
      # calculate split score + weight for each split-point in all leaves
      histograms_per_leave <- ds.split_bins(data_name, current_tree,
                                            data_classes, datasources)
      
      # We search for the best possible split(s) in the newly added branch.
      best_split <- ds.select_split(histograms_per_leave, split_val, data_classes,
                                    reg_par)
      
      if (i == 1) {
        split_scores_left <- data.frame(sp_sc = numeric(), feature = character(),
                                        split_val = numeric(), cont_NA = numeric(),
                                        weight_l = numeric(), weight_r = numeric())
        
        split_scores_right <- data.frame(sp_sc = numeric(), feature = character(),
                                         split_val = numeric(), cont_NA = numeric(),
                                         weight_l = numeric(), weight_r = numeric())
        
        next_split <- list(best_split$feature[[1]], best_split$split_val[[1]],
                           best_split$cont_NA[[1]], TRUE, best_split$weight_l[[1]],
                           TRUE, best_split$weight_r[[1]], 0, TRUE)
        current_tree[1, ] <- next_split
        if (cand_select[["numeric"]] == "ithess") {
          add_par[["hessians"]] <- histograms_per_leave[[1]]$hess
        }
      } else {
        # TODO: Fix rownames for copying rows into df
        split_scores_left[i - 1, ] <- best_split[1, ]
        split_scores_right[i - 1, ] <- best_split[2, ]
        
        max_l_index <- which.max(split_scores_left$sp_sc)
        max_r_index <- which.max(split_scores_right$sp_sc)
        
        max_l <- split_scores_left$sp_sc[[max_l_index]]
        max_r <- split_scores_right$sp_sc[[max_r_index]]
        
        if (max_l > 0 || max_r > 0) {
          if (max_l > max_r) {
            next_split <- split_scores_left[max_l_index, ]
            current_tree[i, ] <- list(next_split$feature[[1]],
                                      next_split$split_val[[1]],
                                      next_split$cont_NA[[1]], TRUE,
                                      next_split$weight_l[[1]], TRUE,
                                      next_split$weight_r[[1]], max_l_index,
                                      TRUE)
            current_tree$w_s_left[[max_l_index]] <- FALSE
            current_tree$w_s_left_value[[max_l_index]] <- i
            split_scores_left$sp_sc[[max_l_index]] <- 0
          } else {
            next_split <- split_scores_right[max_r_index, ]
            current_tree[i, ] <- list(next_split$feature[[1]],
                                      next_split$split_val[[1]],
                                      next_split$cont_NA[[1]], TRUE,
                                      next_split$weight_l[[1]], TRUE,
                                      next_split$weight_r[[1]], max_r_index,
                                      FALSE)
            current_tree$w_s_right[[max_r_index]] <- FALSE
            current_tree$w_s_right_value[[max_r_index]] <- i
            split_scores_right$sp_sc[[max_r_index]] <- 0
          }
        } else {
          break
        }
      }
      # save best one for each leaf and continue with the best split over all available ones.
    } else if (split_method == "totally_random") {
      
      # spp_cand <- spp_cand[setdiff(names(spp_cand), categorical)]
      # choose one feature and split point
      feature <- sample(names(spp_cand), 1)
      split_val <- sample(spp_cand[[feature]], 1)
      if (data_classes[[feature]] == "numeric") {
        cont_NA <- sample(1:2, 1)
      } else {
        cont_NA <- 0
      }
      
      if (i == 1) {
        new_split <- list(feature, split_val, cont_NA, TRUE, 0, TRUE, 0, 0, TRUE)
        current_tree[1, ] <- new_split
      } else {
        for (j in (2^(i - 1)):(2^i - 1)) {
          
          parent_index <- floor(j / 2)
          if (j %% 2 == 0) {
            parent_dir <- TRUE
            current_tree$w_s_left[[parent_index]] <- FALSE
            current_tree$w_s_left_value[[parent_index]] <- j
          } else {
            parent_dir <- FALSE
            current_tree$w_s_right[[parent_index]] <- FALSE
            current_tree$w_s_right_value[[parent_index]] <- j
          }
          new_split <- list(feature, split_val, cont_NA, TRUE, 0, TRUE, 0,
                            parent_index, parent_dir)
          current_tree[j, ] <- new_split
        }
      }
    }
  }
  
  if (split_method == "totally_random") {
    leaf_weights <- ds.update_weight(data_name, current_tree, max_splits,
                                     reg_par, weight_update, datasources)
    
    for (j in 1:(2^(i - 1))) {
      tree_row <- 2^(i - 1) - 1 + j
      current_tree$w_s_left_value[[tree_row]] <- leaf_weights[[2 * j - 1]]
      current_tree$w_s_right_value[[tree_row]] <- leaf_weights[[2 * j]]
    }
  }
  
  ds.save_tree(data_name, current_tree, amt_trees + 1, length(removed_trees),
               datasources)
  if (dropout_rate < 1) {
    ds.update_trees(data_name, removed_trees, amt_trees + 1, datasources)
  }
  
  return(list(current_tree, add_par, removed_trees))
}