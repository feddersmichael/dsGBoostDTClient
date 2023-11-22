
# Generate splitting points
ds.gen_spp_cand <- function(data_name, cand_select_mode = "percentile", 
                              datasources = NULL){
  
  # need name change for training_data
  
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  # exposes a lot of privacy in the Differential Privacy meaning
  # could be used if not existence of certain data points should be kept
  # private but rather in which data set they exist
  # -> especially as we don't know which feature values belong together to
  # one data point
  # risky mainly when one feature can be connected to a certain data set
  # but then even having an accurate model could be privacy revealing
  # afterwards
  if (cand_select_mode == "exact"){
    spp <- ds.exact_spp_cand(data_name)
  }
  # We need to have bounds for the feature space so that we can select a
  # specified amount of splitting candidates. Could be an option to add
  # logarithmic scale as well
  else if (cand_select_mode == "uniform"){
    spp <- ds.uniform_spp_cand(data_name)
  }
  # here we need the Hessian histograms, procedure as describes in Maddock
  else if (cand_select_mode == "ithess"){
    spp <- ds.ithess_spp_cand(data_name)
  }
  else {
    cat(paste0("The specified mode '", cand_select_mode, 
               "' is not supported. \n"))
    if (tolower(readline(prompt = "Do you want to proceed with 
                         the splitting candidates method 'Iterative Hessian'? 
                         (y/n): ")) == "y"){
      spp <- ds.ithess_spp_cand(data_name)
    }
    else {
      stop("Please retry with one of the supported splitting candidate methods.
           \n Currently supported are 'exact', 'uniform' and 'ithess'. \n")
    }
  }
  data
  mode
  return(spp)
}