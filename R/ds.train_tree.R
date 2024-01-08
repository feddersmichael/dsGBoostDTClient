
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
  
  split_scores <- list()
  
  # In this loop we build a tree with up to 'max_splits' many splits.
  for (i in 1:max_splits){
    
    hist_bins_per_leave <- ds.split_bins(data_name, min_max, current_tree,
                                         spp_cand, current_tree, data_type,
                                         datasources)
    
    # We search for the best possible split(s) in the newly added branch.
    best_split <- ds.select_split(hist_bins_per_leave, spp_cand)
    
    if (nrow(current_tree) == 0){
      first_split <- c(best_split$feature[1], best_split$split_pt[1],
                       TRUE, best_split$weight_l[1], TRUE,
                       best_split$weight_r[1])
      current_tree <- rbind(current_tree, first_split)
    }
    else {
      split_scores <- append(split_scores, best_split)
      
    }
    
  }
  
  return(current_tree)
}