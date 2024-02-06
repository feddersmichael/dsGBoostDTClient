
#' Shrink tree
#'
#' @param tree The input tree.
#' @param shrinkage The factor by which the weights get multiplied.
#'
#' @return Teh changed tree
#' @export
ds.add_shrinkage <- function(tree, shrinkage) {
  
  for (i in seq_len(nrow(tree))) {
    if (tree$w_s_left[i]) {
      tree$w_s_left_value[i] <- shrinkage * tree$w_s_left_value[i]
    }
    if (tree$w_s_right[i]) {
      tree$w_s_right_value[i] <- shrinkage * tree$w_s_right_value[i]
    }
  }
  
  return(tree)
}