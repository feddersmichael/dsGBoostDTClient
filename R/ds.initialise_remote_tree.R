
#' First training round of a remote tree.
#'
#' @param data_name The name under which the data is saved on the server.
#' @param federation Through which method we share the data between client and
#' the servers.
#' @param weight_update Through which method we choose the weights for our tree.
#' @param dropout_rate Chance that a tree is not used for building the next
#' tree.
#' @param shrinkage The shrinkage factor for the tree.
#' @param cand_select Splitting-point selection for numeric and factor features.
#' @param ithess_stop Maximum amount of times we update the split-point
#' candidates if the split-method is "totally_random".
#' @param split_method Through which method we choose the tree-splits.
#' @param reg_par Regularisation parameter which prevent overfitting.
#' @param feature_subsampling Which part of the feature space we use to build
#' the trees.
#' @param data_classes Data class for all features.
#' @param amt_spp The amount of splitting point candidates per feature.
#' @param max_splits The maximum amount of splits in the trained tree.
#' @param datasources DATASHIELD server connection.
#'
#' @return The trained Tree
#' @export
ds.initialise_remote_tree <- function(data_name, federation, weight_update,
                                      dropout_rate, shrinkage, cand_select,
                                      ithess_stop, split_method, reg_par,
                                      feature_subsampling, data_classes,
                                      amt_spp, max_splits, datasources) {
  
  amt_server <- length(datasources)
  if(federation[["mode"]] == "trees_cyclical") {
    selected_server <- 1:federation[["selection"]]
  } else if(federation[["mode"]] == "trees_random") {
    if (federation[["selection"]] < 1) {
      selected_server_amt <- stats::rbinom(1, amt_server, federation[["selection"]])
      if(selected_server_amt == 0) {
        selected_server_amt <- 1L
      }
      selected_server <- sample(1:amt_server, selected_server_amt)
    } else {
      selected_server <- sample(1:amt_server, federation[["selection"]])
    }
  }
  
  if (is.null(feature_subsampling)) {
    selected_feat <- names(data_classes)
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
  
  removed_trees <- integer()
  spp_cand <- list()
  save_list <- list(weight_update = weight_update, dropout_rate = dropout_rate,
                    removed_trees = removed_trees, cand_select = cand_select,
                    comunication_round = 1L, ithess_stop  = ithess_stop,
                    reg_par = reg_par, selected_feat = selected_feat,
                    amt_spp = amt_spp, spp_cand = spp_cand,
                    max_splits = max_splits)
  exist_check <- c(weight_update = TRUE, dropout_rate = TRUE,
                   removed_trees = TRUE, cand_select = TRUE,
                   comunication_round = TRUE, ithess_stop = TRUE,
                   reg_par = TRUE, selected_feat = TRUE,
                   amt_spp = TRUE, spp_cand = TRUE, max_splits = TRUE)
  ds.save_variables(data_name, save_list, exist_check, datasources)
  
  cally <- call("calc_hist_initDS", data_name)
  DSI::datashield.assign.expr(datasources, paste0(data_name, "_training"), cally)
  
  if (cand_select[["numeric"]] == "ithess") {
    cand_select <- list(numeric = "uniform", factor = cand_select[["factor"]])
    cally <- call("gen_spp_candDS", data_name, cand_select)
    DSI::datashield.assign.expr(datasources, paste0(data_name, "_spp_cand"), cally)
  } else {
    cally <- call("gen_spp_candDS", data_name, cand_select)
    DSI::datashield.assign.expr(datasources[selected_server],
                                paste0(data_name, "_spp_cand"), cally)
  }
  
  cally <- call("train_treeDS", data_name)
  trees <- DSI::datashield.aggregate(datasources[selected_server], cally)
  
  amt_trees <- length(trees)
  if (amt_trees > 1) {
    for (i in 1:amt_trees) {
      if (shrinkage < 1) {
        ds.save_tree(data_name, ds.add_shrinkage(trees[[i]], shrinkage), i, 0, amt_trees, datasources)
      } else {
        ds.save_tree(data_name, trees[[i]], i, 0, amt_trees, datasources)
      }
    }
  } else {
    if (shrinkage < 1) {
      ds.save_tree(data_name, ds.add_shrinkage(trees[[1]], shrinkage), 1L, 0, 1L, datasources)
    } else {
      ds.save_tree(data_name, trees[[1]], 1L, 0, 1L, datasources)
    }
  }
  
  ds.update_trees(data_name, removed_trees, 1:amt_trees, datasources)
  
  return(trees)
}