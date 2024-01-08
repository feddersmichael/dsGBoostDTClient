
#' We want to check in a generic way if the uploaded data fulfills
#  our requirements to be used in this analysis.
#'
#' @param data_name 
#' @param split_ratio 
#' @param only_numeric 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.data_format_check <- function(data_name, split_ratio, only_numeric = FALSE, 
                                 datasources = NULL){
  # TODO: dimension check between servers
  
  #TODO needs clarification how exactly the right data source is checked.
  if (is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # We check if 'data_name' is of type character
  if (!is.character(data_name)){
    stop("'data_name' needs to be a character with the name of the data file.")
  }
  
  # Now we want to see on all servers an object with name 'data_name' exists.
  if (!dsBaseClient::ds.exists(data_name, datasources)){
    stop(paste0("There doesn't exist a data file with the data_name ", data_name))
  }
  
  # In the next step we check if the file with this name is a data frame
  if (!"data.frame" %in%  dsBaseClient::ds.class(data_name, datasources)){
    stop(paste0("The data needs to be saved as a data frame object instead of ", 
                dsBaseClient::ds.class(data_name, datasources)))
  }
  
  # If we activate the numeric option we need to check if all entries of
  # the data frame are numeric.
  if (only_numeric){
    if(!ds.data_class_numeric(data_name, datasources)){
      stop("All columns in our data need to be continuous numeric values.")
    }
  }
  
}

