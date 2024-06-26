
#' Split the data into 'Test' and 'Training'
#'
#' @param data_name The name under which the data is saved on the server.
#' @param train_test_ratio Percentage of the data which should be used for
#' Training.
#' @param datasources DATASHIELD server connection.
#'
#' @return None.
#' @export
ds.create_data_split <- function(data_name, train_test_ratio = 0.9,
                                 datasources = NULL) {
  # TODO: keep certain characteristics similar in train and test

  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }

  # We split up the data set in a training and test part.
  cally <- call("create_data_splitDS", data_name, train_test_ratio)
  DSI::datashield.assign.expr(datasources,
                              paste0(data_name, "_training_test_split"), cally)

}