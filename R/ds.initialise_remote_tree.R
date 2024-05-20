
ds.initialise_remote_tree <- function(data_name, federation, weight_update,
                                      dropout_rate, cand_select, ithess_stop,
                                      split_method, reg_par, amt_server,
                                      feature_subsampling, datasources) {
  
  
  if(federation[["mode"]] == "cyclical") {
    selected_server <- 1:federation[["selection"]]
  } else if(federation[["mode"]] == "random") {
    if (federation[["selection"]] < 1) {
      selected_server_amt <- stats::rbinom(1, amt_server, federation[["selection"]])
      if(selected_server_amt == 0) {
        selected_server_amt <- 1
      }
      selected_server <- sample(1:amt_server, selected_server_amt)
    } else {
      selected_server <- sample(1:amt_server, federation[["selection"]])
    }
  }
  
  if (is.null(feature_subsampling)) {
    selected_feat <- NULL
  } else {
    if (feature_subsampling[["mode"]] == "cyclical") {
      selected_feat <- names(data_classes)[1:feature_subsampling[["selection"]]]
    } else if (feature_subsampling[["mode"]] == "random") {
      if (feature_subsampling[["selection"]] < 1) {
        amt_feat <- stats::rbinom(1, length(data_classes), feature_subsampling[["selection"]])
        if (amt_feat == 0) {
          amt_feat <- 1L
        }
        selected_feat <- sample(names(data_classes), amt_feat)
      } else {
        selected_feat <- sample(names(data_classes), feature_subsampling[["selection"]])
      }
    }
  }
  
  removed_trees <- c()
  save_list <- list(weight_update = weight_update, dropout_rate = dropout_rate,
                    removed_trees = removed_trees, cand_select = cand_select,
                    comunication_round = 1L, ithess_stop  = ithess_stop,
                    reg_par = reg_par, selected_feat = selected_feat)
  exist_check <- c(weight_update = TRUE, dropout_rate = TRUE,
                   removed_trees = TRUE, cand_select = TRUE,
                   comunication_round = TRUE, ithess_stop = TRUE,
                   reg_par = TRUE, selected_feat = TRUE)
  ds.save_variables(data_name, save_list, exist_check, datasources)
  
  cally <- call("calc_hist_initDS", data_name)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  
  if (cand_select[["numeric"]] == "ithess") {
    cand_select <- list(numeric = "uniform", factor = cand_select[["factor"]])
  }
  cally <- call("gen_spp_candDS", data_name, cand_select)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_spp_cand"), cally)
  
  cally <- call("train_treeDS", data_name)
  trees <- DSI::datashield.aggregate(datasources, cally)
  
  amt_trees <- length(trees)
  
  if (amt_trees > 1) {
    for (i in 1:amt_trees) {
      ds.save_tree(data_name, ds.add_shrinkage(trees[[i]], 1 / amt_trees), i, 0,
                   datasources)
    }
  } else {
    ds.save_tree(data_name, trees[[1]], 1, 0, datasources)
  }
  
  ds.update_trees(data_name, removed_trees, 1:amt_trees, datasources)
  
  return(trees)
}