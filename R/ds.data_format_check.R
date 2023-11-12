# We want to check in a generic way if the uploaded data fulfills
# our requirements to be used in this analysis.

ds.data_format_check <- function(name, split_ratio, only_numeric = FALSE, datasources = NULL){
  
  #TODO needs clarification how exactly the right datasource is checked.
  if (is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # We check if 'name' is of type character
  if (!is.character(name)){
    stop("'name' needs to be a character with the name of the data file.")
  }
  
  # Now we want to see on all servers an object with name 'name' exists.
  if (!dsBaseClient::ds.exists(name, datasources)){
    stop(paste0("There doesn't exist a data file with the name ", name))
  }
  
  # In the next step we check if the file with this name is a data frame
  if (!"data.frame" %in%  dsBaseClient::ds.class(name, datasources)){
    stop(paste0("The data needs to be saved as a data frame object instead of ", 
                dsBaseClient::ds.class(name, datasources)))
  }
  
  # If we activate the numeric option we need to check if all entries of
  # the data frame are numeric.
  if (only_numeric){
    if(!ds.data_class_numeric(name, datasources)){
      stop("All columns in our data need to be continuous numeric values.")
    }
  }
  
}

