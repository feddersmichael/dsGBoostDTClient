
ds.update_weight <- function(data_name, current_tree, max_splits, weight_update,
                             datasources) {
  
  # We first check all the inputs for appropriate class and set defaults if
  # no input is given.
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  if (!(is.list(datasources) && all(unlist(lapply(datasources,
                                                  function(d) {methods::is(d, "DSConnection")}))))) {
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call. = FALSE)
  }
  
  cally <- call("update_weightDS", data_name, current_tree, max_splits, weight_update)
  weights <- DSI::datashield.aggregate(datasources, cally)
}