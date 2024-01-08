
#' ds.train_tree
#'
#' @param data_name 
#' @param last_tr_tree 
#' @param max_splits 
#' @param spp_cand 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.train_tree <- function(data_name, last_tr_tree, max_splits = 10,
                          spp_cand = NULL, datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.character(data_name)){
    stop("'data_name' needs to have data type 'character'.")
  }
  
  if (!is.data.frame(last_tr_tree) && !is.null(last_tr_tree)){
    stop("'last_tr_tree' needs to be an object of type 'data frame'.")
  }
  
  if (!is.integer(max_splits)){
    stop("'max_splits' needs to have data type 'integer'.")
  }
  
  # We first update the histogram values, which are based on the previously
  # trained trees.
  
  ds.calc_hist(data_name, last_tr_tree, loss_function, datasources)
  
  current_tree <- data.frame(Feature = numeric(), split_value = numeric(),
                             w_s_left = logical(), w_s_left_value = numeric(),
                             w_s_right = logical(), w_s_right_value = numeric(),
                             par_spp = numeric(), par_dir = logical())
  
  split_scores_left <- data.frame(sp_sc = numeric(), feature = numeric(),
                                  split_val = numeric(), cont_NA = numeric(),
                                  weight_l = numeric(), weight_r = numeric())
  
  split_scores_right <- data.frame(sp_sc = numeric(), feature = numeric(),
                                  split_val = numeric(), cont_NA = numeric(),
                                  weight_l = numeric(), weight_r = numeric())
  
  # In this loop we build a tree with up to 'max_splits' many splits.
  for (i in 1:max_splits){
    
    hist_bins_per_leave <- ds.split_bins(data_name, min_max, current_tree,
                                         spp_cand, current_tree, data_type,
                                         datasources)
    
    # We search for the best possible split(s) in the newly added branch.
    best_split <- ds.select_split(hist_bins_per_leave, spp_cand)
    
    if (nrow(current_tree) == 0){
      first_split <- c(best_split$feature[1], best_split$split_val[1],
                       TRUE, best_split$weight_l[1], TRUE,
                       best_split$weight_r[1], 0, TRUE)
      current_tree <- rbind(current_tree, first_split)
    }
    else {
      split_scores_left <- rbind(split_scores_left, best_split[[1]])
      split_scores_right <- rbind(split_scores_right, best_split[[2]])
      
      max_l <- which.max(split_scores_left$sp_sc)
      max_r <- which.max(split_scores_right$sp_sc)
      
      if (split_scores_left$sp_sc[max_l] > split_scores_right$sp_sc[max_r]){
        next_split <- split_scores_left[max_l, ]
        current_tree <- rbind(current_tree, c(next_split[2], next_split[3],
                                              TRUE, next_split[5], TRUE,
                                              next_split[6], max_l, TRUE))
      }
      else {
        next_split <- split_scores_right[max_r, ]
        current_tree <- rbind(current_tree, c(next_split[2], next_split[3],
                                              TRUE, next_split[5], TRUE,
                                              next_split[6], max_r, FALSE))
      }
    }
    
  }
  
  return(current_tree)
}