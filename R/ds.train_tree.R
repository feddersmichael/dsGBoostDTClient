
ds.train_tree <- function(boosted_tree, max_splits = 10, datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.data.frame(boosted_tree)){
    stop("'boosted_tree' needs to be an object of type 'data frame'.")
  }
  
  if (!is.integer(max_splits)){
    stop("'max_splits' needs to have data type 'integer'.")
  }
  
  # We need to store the predicted output value for all data points in the 
  # training set on the server to calculate the histograms
  ds.output_pred(boosted_tree[[length(boosted_tree)]], datasources)
  
  
  # In this we loop we split up to 'max_splits' amount of times.
  # If the function 'ds.select_split' returns a break criteria instead of a new
  # split we stop the loop and return the finished tree.
  
  for (i in 1:max_splits){
    
    # We search for the next split point.
    split <- ds.select_split()
    
    #
    if (is.character(split)){
      break
    }
    else {
      ds.add_split()
    }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # TODO: do we need to update spp every round or just at the start? if they are
  # permanent we can save them directly on the server
  # maybe sth like spliting spp_cand methods in group global and local and
  # change algorithm based on that
  spp_cand <- ds.gen_spp_cand(data_name, cand_select_mode, datasources)

  tree <- matrix(nrow = 0, ncol = 4)
  
  while (TRUE) {
    # Probably better to find a way to update split points in the one feature
    # where we added a split -> the rest should stay the same
    spp_cand <- ds.gen_spp(data_name, cand_select_mode, datasources)
    
    # ---
    # from original big exact splitting point function
    # ---
    # from server need y_i and \hat{y}_i and maybe data security level
    
    # check for data sensibility at server
    # 1: data can be shared
    # 2: histograms can be shared
    # 3: only optimal splitting point on local data is shared
    
    # Tasks which need to be performed:
    # go through all possible splits
    # calculate split score fore each split
    #   this include summing up histograms -> Secure Multi-Party Computation (MPC)
    # save all or only highest... ?
    # select highest split to expand tree
    
    ds.add_split()
    
    hist <- ds.calc_hist()
    
    # Probably just different feedback if ssc <= 0 instead of sending back all
    # Shall send back always the best combination. If that isn't good enough
    # we can just end tree building process.
    split_list <- ds.calc_spsc()
    
    if (ssc > 0){
      
      # probably faster to only change affected rows instead of copying 
      # everything
      tree <- ds.add_split(split_list[2:4], tree)
      # TODO How and if data is affected by splitting for effective programming
      
    }
    # More criteria like fixed max amount of iterations or sth possible
    else {
      break
    }
    
  }
  
  # Now we can give back the tree
  
  return(tree)
}