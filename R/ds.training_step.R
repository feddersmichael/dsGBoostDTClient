
ds.training_step <- function(data, spp_mode, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  spp <- ds.gen_spp(data, spp_mode, datasources)

  tree <- matrix(nrow = 0, ncol = 4)
  
  while (TRUE) {
    # Probably better to find a way to update split points in the one feature
    # where we added a split -> the rest should stay the same
    spp <- ds.gen_spp(data, spp_mode, datasources)
    
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
    
    hist <- ds.calc_hist()
    
    # Probably just different feedback if ssc <= 0 instead of sending back all
    # Shall send back always the best combination. If that isn't good enough
    # we can just end tree building process.
    split_list <- ds.calc_spsc()
    
    if (ssc > 0){
      
      # probably faster to only change affected rows instead of copying 
      # everything
      tree <- ds.add_spp(split_list[2:4], tree)
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