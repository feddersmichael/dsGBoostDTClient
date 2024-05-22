
ds.train_remote_tree <- function(data_name, federation, comunication_round, prev_amt_trees,
                                 feature_subsampling, data_classes, dropout_rate,
                                 datasources) {
  
  amt_server <- length(datasources)
  
  if(federation[["mode"]] == "cyclical") {
    selected_server <- ((((comunication_round - 1) * federation[["selection"]]) %% amt_server) + 1):
      (((comunication_round * federation[["selection"]] - 1) %% amt_server) + 1)
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
      selected_feat <- names(data_classes)[((((comunication_round - 1) * feature_subsampling[["selection"]]) %% length(data_classes)) + 1):
                                             (((comunication_round * feature_subsampling[["selection"]] - 1) %% length(data_classes)) + 1)]
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
  save_list <- list(selected_feat = selected_feat)
  exist_check <- c(selected_feat = FALSE)
  ds.save_variables(data_name, save_list, exist_check, datasources)
  
  if (dropout_rate > 0) {
    amt_removed_trees <- stats::rbinom(1, prev_amt_trees, dropout_rate)
    if (amt_removed_trees == 0) {
      amt_removed_trees <- 1
    }
    removed_trees <- sample.int(prev_amt_trees, amt_removed_trees)
  } else {
    removed_trees <- integer()
  }
  
  cally <- call("calc_histDS", data_name, removed_trees)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  
  cally <- call("gen_spp_candDS", data_name)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_spp_cand"), cally)
  
  cally <- call("train_treeDS", data_name)
  trees <- DSI::datashield.aggregate(datasources, cally)
  
  amt_trees <- length(trees)
  
  if (amt_trees > 1) {
    for (i in 1:amt_trees) {
      ds.save_tree(data_name, ds.add_shrinkage(trees[[i]], 1 / amt_trees),
                   prev_amt_trees + i, length(removed_trees), datasources)
    }
  } else {
    ds.save_tree(data_name, trees[[1]], prev_amt_trees + 1,
                 length(removed_trees), datasources)
  }
  
  ds.update_trees(data_name, removed_trees,
                  (prev_amt_trees + 1):(prev_amt_trees + amt_trees),
                  datasources)
  
  return(trees)
}