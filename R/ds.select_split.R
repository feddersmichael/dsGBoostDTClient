
# TODO: Structure which variables should be mentioned explicitly and which
# ones only under a general variable-list
ds.select_split <- function(spp_cand = NULL, amt_spp = NULL, min_max = NULL, 
                            datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.list(min_max)){
    stop("'min_max' needs to be an object of type 'list'.")
  }
  
  if (!is.list(amt_spp)){
    stop("'amt_spp' needs to be an object of type 'list'.")
  }
  
  # To generate the splitting-point candidates we need to have min_max values
  # for each feature for which we want to create 'amt_spp' splitting points.
  if (!(length(amt_spp) == length(min_max))){
    stop("'amt_spp' and 'min_max' need to have the same length.")
  }
  
  # If we don't have splitting-point candidates we need to generate them.
  if (is.null(spp_cand)){
    spp_cand <- ds.gen_spp_cand(amt_spp, min_max, "uniform", datasources)
  }
  else if (!is.list(spp_cand)){
    stop("'spp_cand' needs to be an object of type 'list'.")
  }
  
  # Now we need to calculate the histograms for all splitting-point candidates
  # in all leaves.
  histograms <- ds.calc_hist(data_name, loss_function, data_type, spp_cand, 
                             datasources)
  
  # Now we can calculate the split score for all possibilities.
  spscores <- ds.calc_spsc(histograms)
  
  # From all split scores we can now choose the best split.
  best_split <- ds.best_split()
  
  # Finally we return our choice.
  return(best_split)
}