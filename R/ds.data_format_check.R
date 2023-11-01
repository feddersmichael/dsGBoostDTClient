# We want to check in a generic way if the uploaded data fulfills
# our requirements to be used in this analysis.

ds.data_format_check <- function(name, datasources = NULL){
  
  #TODO needs clarification how exactly the right datasource is checked.
  if (is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # We check if the name is of type character
  if (!is.character(name)){
    stop("'name' needs to be a character with the name of the data file.")
  }
  
  # Now we want to see on all servers an object with name 'name' exists.
  dsBaseClient::ds.testObjExists(name, datasources)
  
  # In the next step we check if the file with this name has the right format

}

