
ds.train_tree <- function(max_treecount, regul_par, datasources = NULL){
  
  if (!class(max_treecount) == "Integer"){
    stop("max_treecount needs to be an integer.")
  }
  
  # We save our tree in a nx4 matrix.
  
  # The first two columns save the split points.
  # Each row represents one split point with the first column showing the
  # feature along which we split (5th e.g.) and the second column showing the
  # splitting condition
  
  # The last two columns denote for the right and left path either the row
  # number under which we find their splitting condition if we have further
  # splits or the weight if the tree ends.
  tree <- matrix(nrow = 0, ncol = 4)
  
  for (i in 1:max_treecount){
    tree <- ds.training_step()
  }
}