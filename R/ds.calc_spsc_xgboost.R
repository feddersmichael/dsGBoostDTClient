
ds.calc_spsc_xgboost <- function(data_name, tree, spp_cand, lambda, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  # we need the histogram sums
  # TODO: possibility of Secure Multi-Party Computation, otherwise we can just
  # send the histogram sums for all buckets
  cally <- call("calc_histDS", data_name, spp_cand, "quadratic")
  hist <- DSI::datashield.aggregate(datasources, cally)
  
  # In a first step we need to sum up the values from the different servers
  
  reduce_hist <- function(S_1, S_2){
    
    mapply(function(F_1, F_2){return(F_1 + F_2)}, S_1, S_2)
  }
  
  # first derivative histograms
  hist_1 <- Reduce(reduce_hist, hist[[1]])
  # second derivative histograms
  hist_2 <- Reduce(reduce_hist, hist[[2]])
  
  # We can now calculate the total sum of 1st and 2nd order histograms over
  # all data points
  G <- sum(hist_1[[1]])
  H <- sum(hist_2[[1]])
  
  
  
}