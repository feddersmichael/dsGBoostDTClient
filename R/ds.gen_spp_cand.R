
# Idea: if we want to introduce other 'cand_select_mode' which needs different
# parameters than 'amt_spp' and 'min_max' we ask for a general list 'parameters'
# and then just pass the elements through 'parameters[[1]]', 'parameters[[2]]'
# etc.
# Idea: add mode for logarithmic scale e.g. for uniform
# TODO: implement iterative hessian
ds.gen_spp_cand <- function(amt_spp, min_max, cand_select_mode = "percentile", 
                              datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (!is.character(cand_select_mode)){
    stop("'cand_select_mode' needs to have data type 'character'.")
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
  
  # For each type of candidate-selection we call a different function.
  if (cand_select_mode == "uniform"){
    spp_cand <- ds.uniform_spp_cand(amt_spp, min_max)
  }
  # exposes a lot of privacy in the Differential Privacy meaning
  # could be used if not existence of certain data points should be kept
  # private but rather in which data set they exist
  # -> especially as we don't know which feature values belong together to
  # one data point
  # risky mainly when one feature can be connected to a certain data set
  # but then even having an accurate model could be privacy revealing
  # afterwards
  else if (cand_select_mode == "exact"){
     spp_cand <- ds.exact_spp_cand(data_name)
  }
  # here we need the Hessian histograms, procedure as describes in Maddock
  else if (cand_select_mode == "ithess"){
    spp_cand <- ds.ithess_spp_cand(data_name)
  }
  else {
    cat(paste0("The specified mode '", cand_select_mode, 
               "' is not supported. \n"))
    if (tolower(readline(prompt = "Do you want to proceed with 
                         the splitting candidates method 'Iterative Hessian'? 
                         (y/n): ")) == "y"){
      spp_cand <- ds.ithess_spp_cand(data_name)
    }
    else {
      stop("Please retry with one of the supported splitting candidate methods.
           \n Currently supported are 'exact', 'uniform' and 'ithess'. \n")
    }
  }
  
  return(spp_cand)
}