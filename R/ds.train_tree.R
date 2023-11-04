
ds.train_tree <- function(max_treecount, regul_par, datasources = NULL){
  
  if (!class(max_treecount) == "Integer"){
    stop("max_treecount needs to be an integer.")
  }
  
  tree <- ds.start_tree()
  
  for (i in 1:max_treecount){
    
  }
}