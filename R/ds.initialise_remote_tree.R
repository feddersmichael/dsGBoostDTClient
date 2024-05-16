
ds.initialise_remote_tree <- function(data_name, federation, weight_update,
                                      dropout_rate, cand_select, ithess_stop,
                                      split_method, reg_par) {
  
  # Dummy
  if(federation[["mode"]] == "cyclical") {
    selected_server <- 1
  } else if(federation[["mode"]] == "random") {
    selected_server <- 1
  }
  
  removed_trees <- c()
  if (cand_select[["numeric"]] == "ithess") {
    cand_select <- list(numeric = "uniform", factor = cand_select[["factor"]])
  }
  save_list <- list(weight_update = weight_update, dropout_rate = dropout_rate,
                    removed_trees = removed_trees, cand_select = cand_select,
                    comunication_round = 1L, ithess_stop  = ithess_stop,
                    reg_par = reg_par)
  exist_check <- c(weight_update = TRUE, dropout_rate = TRUE,
                   removed_trees = TRUE, cand_select = TRUE,
                   comunication_round = TRUE, ithess_stop = TRUE,
                   reg_par = TRUE)
  ds.save_variables(data_name, save_list, exist_check, datasources)
  
  cally <- call("calc_hist_initDS", data_name)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  
  cally <- call("gen_spp_candDS", data_name)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_spp_cand"), cally)
  
  cally <- call("train_treeDS", data_name)
  trees <- DSI::datashield.aggregate(datasources, cally)
  
  return(trees)
}