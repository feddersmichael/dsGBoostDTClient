
#' Split the data into 'Test' and 'Training'
#'
#' @param data_name The name under which the data is saved on the server.
#' @param train_test_ratio Percentage of the data which should be used for 
#' Training.
#' @param split_status Defines if 'data_name' saves the full data, is already
#' split up into Training and Test data or saves only one of them.
#' @param datasources DATASHIELD server connection.
#'
#' @return None.
#' @export
ds.create_data_split <- function(data_name, train_test_ratio, 
                                 split_status = NULL, datasources = NULL){
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  } 
  else if (!DSI:::.isDSConnection(datasources)) {
    stop("'datasources' needs to be a an object of the 'DSConnection' class.")
  }
  
  if (is.null(split_status)){
    if (!is.character(data_name) || !(length(data_name) != 1)){
      stop("'data_name' needs to be an atomic vector with data type 'character'.")
    }
    
    if (!is.numeric(train_test_ratio) || (numeric(train_test_ratio) < 0) || 
        (numeric(train_test_ratio) > 1)){
      stop(paste0("'train_test_ratio' needs to have data type 'numeric' and lie",
                  " between 0 and 1."))
    }
    
    # We split up the dataset in a training and test part.
    cally <- call("create_data_splitDS", data_name, train_test_ratio,
                  split_status)
    result <- DSI::datashield.assign.expr(datasources, 
                                          paste0(data_name,
                                                 "_training_test_split"), 
                                          cally)
    
    cally <- call("save_trainingDS", data_name)
    result <- DSI::datashield.assign.expr(datasources, paste0(data_name,
                                                              "_training"), 
                                          cally)
  }
  else if (split_status == 'Train'){
    if (!is.character(data_name) || !(length(data_name) != 2)){
      stop("'data_name' needs to be an object of type 'vector' with data type 'character' and length 2.")
    }
    
    # We save the training data-set under a general name.
    cally <- call("create_data_splitDS", data_name[1], train_test_ratio,
                  split_status)
    result <- DSI::datashield.assign.expr(datasources, 
                                          paste0(data_name[2],
                                                 "_training_test_split"), 
                                          cally)
    
    cally <- call("save_trainingDS", data_name)
    result <- DSI::datashield.assign.expr(datasources, paste0(data_name[2],
                                                              "_training"), 
                                          cally)
  }
  else if (split_status == 'Train_Test'){
    if (!is.character(data_name) || !(length(data_name) != 3)){
      stop("'data_name' needs to be an object of type 'vector' with data type 'character' and length 3.")
    }
    
    # We save the training data-set under a general name.
    cally <- call("create_data_splitDS", data_name[1:2],
                  train_test_ratio, split_status)
    result <- DSI::datashield.assign.expr(datasources, 
                                          paste0(data_name[3],
                                                 "_training_test_split"), 
                                          cally)
    
    cally <- call("save_trainingDS", data_name)
    result <- DSI::datashield.assign.expr(datasources, paste0(data_name[3],
                                                              "_training"), 
                                          cally)
  }
  else {
    stop("'split_status' needs to be either 'NULL' or an atomic vector with data type 'character'.")
  }
  
}