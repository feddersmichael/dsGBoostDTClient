
#' Train a single tree
#'
#' @param data_name The name under which the data is saved on the server.
#' @param last_tr_tree The last trained tree.
#' @param bounds_and_levels The maximum and minimum values for numeric features
#' and levels for factor features.
#' @param data_classes Data class for all features.
#' @param output_var Name of the output variable.
#' @param loss_function The type of loss function under which we optimise the
#' tree.
#' @param amt_spp The amount of splitting point candidates per feature.
#' @param cand_select Splitting-point selection for numeric and factor features.
#' @param reg_par Regularisation parameter which prevent overfitting.
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param datasources DATASHIELD server connection.
#'
#' @return The trained tree.
#' @export
ds.train_tree <- function(data_name, last_tr_tree, bounds_and_levels,
                          data_classes, output_var, loss_function, amt_spp,
                          cand_select, reg_par = c(5, 5), max_splits = 5, 
                          datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!all(sapply(datasources, DSI:::.isDSConnection))) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.null(last_tr_tree) && !is.data.frame(last_tr_tree)){
    stop("'last_tr_tree' needs to be an object of type 'data frame'.")
  }
  
  if (!is.integer(max_splits) || length(max_splits) != 1) {
    stop("'max_splits' needs to be an atomic 'integer' vector.")
  }
  
  # We first update the histogram values, which are based on the previously
  # trained trees.
  ds.calc_hist(data_name, last_tr_tree, data_classes, output_var, loss_function, 
               datasources)
  
  spp_cand <- ds.gen_spp_cand(bounds_and_levels, data_classes, cand_select,
                              amt_spp)
  
  # We save our tree in a (amount of splits)x8 data frame. Each row represents
  # one split point.
  
  # Column 1 denotes the feature along which we split by name as a character.
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
  
  current_tree <- data.frame(feature = character(), split_value = numeric(),
                             cont_NA = numeric(), w_s_left = logical(),
                             w_s_left_value = numeric(), w_s_right = logical(),
                             w_s_right_value = numeric(), par_spp = numeric(),
                             par_dir = logical())
  
  split_scores_left <- data.frame(sp_sc = numeric(), feature = character(),
                                  split_val = numeric(), cont_NA = numeric(),
                                  weight_l = numeric(), weight_r = numeric())
  
  split_scores_right <- data.frame(sp_sc = numeric(), feature = character(),
                                  split_val = numeric(), cont_NA = numeric(),
                                  weight_l = numeric(), weight_r = numeric())
  
  # In this loop we build a tree with up to 'max_splits' many splits.
  for (i in 1:max_splits){
    histograms_per_leave <- ds.split_bins(data_name, bounds_and_levels,
                                          spp_cand, current_tree, data_classes,
                                          datasources)
    
    # We search for the best possible split(s) in the newly added branch.
    best_split <- ds.select_split(histograms_per_leave, spp_cand, reg_par)
    
    amt_splits <- nrow(current_tree)
    
    if (amt_splits == 0){
      first_split <- list(best_split$feature[1], best_split$split_val[1],
                       best_split$cont_NA[1], TRUE, best_split$weight_l[1],
                       TRUE, best_split$weight_r[1], 0, TRUE)
      current_tree[1, ] <- first_split
    }
    else {
      # TODO: Fix rownames for copying rows into df
      split_scores_left[amt_splits, ] <- best_split[1, ]
      split_scores_right[amt_splits, ] <- best_split[2, ]
      
      max_l_index <- which.max(split_scores_left$sp_sc)
      max_r_index <- which.max(split_scores_right$sp_sc)
      
      max_l <- split_scores_left$sp_sc[max_l_index]
      max_r <- split_scores_right$sp_sc[max_r_index]
      
      if (max_l > 0 || max_r > 0) {
        if (max_l > max_r) {
          next_split <- split_scores_left[max_l_index, ]
          current_tree[amt_splits + 1, ] <- list(next_split$feature[1],
                                              next_split$split_val[1],
                                              next_split$cont_NA[1], TRUE,
                                              next_split$weight_l[1], TRUE,
                                              next_split$weight_r[1],
                                              max_l_index, TRUE)
          current_tree$w_s_left[max_l_index] <- FALSE
          current_tree$w_s_left_value[max_l_index] <- amt_splits + 1
          split_scores_left$sp_sc[max_l_index] <- 0
        }
        else {
          next_split <- split_scores_right[max_r_index, ]
          current_tree[amt_splits + 1, ] <- list(next_split$feature[1],
                                              next_split$split_val[1],
                                              next_split$cont_NA[1], TRUE,
                                              next_split$weight_l[1], TRUE,
                                              next_split$weight_r[1],
                                              max_r_index, FALSE)
          current_tree$w_s_right[max_r_index] <- FALSE
          current_tree$w_s_right_value[max_r_index] <- amt_splits + 1
          split_scores_right$sp_sc[max_r_index] <- 0
        }
      }
      else {
        break
      }
    }
  }
  
  return(current_tree)
}