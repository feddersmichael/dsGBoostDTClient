
#' Creation of the splitting-points for unordered factor features.
#'
#' @param amt_levels The amount of levels this feature has.
#' @param amt_spp The amount of splitting-points which shall be created.
#' @param selection_method By which method the splitting-points shall be chosen.
#'
#' @return The created splitting-points.
#' @export
ds.gen_unord_factor_spp_cand <- function(amt_levels, amt_spp, selection_method) {
  
  if (selection_method == "pivot_switch") {
    
    pivot_points <- c(0, amt_levels / 2)
    if (amt_spp %% 2 == 0) {
      amt_spp <- amt_spp / 2 - 1
      spp_cand <- c(amt_spp / 2 - 0.5)
    } else {
      amt_spp <- (amt_spp - 1) / 2
      spp_cand <- c((amt_spp - 1) / 2)
    }
    
    highest_poss_spp <- amt_levels - 1
    if (amt_spp > highest_poss_spp) {
      warning("'amt_spp' can't be higher than the available amount of levels minus one. It has been reduced to that number.")
      
      amt_spp <- highest_poss_spp
      spp_cand <- 1:highest_poss_spp + 0.5
    } else if (amt_spp == highest_poss_spp) {
      spp_cand <- 1:highest_poss_spp + 0.5
    } else {
      spp_cand <- sort(c(sample(highest_poss_spp, amt_spp) + 0.5, spp_cand))
    }
    
    return(list(spp_cand = spp_cand, pivot_points = pivot_points))
  }
}