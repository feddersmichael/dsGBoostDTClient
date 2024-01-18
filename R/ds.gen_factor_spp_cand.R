
ds.gen_factor_spp_cand <- function(level_set, amt_spp, selection_method) {

  spp_cand <- numeric()

  highest_poss_spp <- length(level_set) - 1
  if (amt_spp > highest_poss_spp) {
    warning(paste0("'amt_spp' can't be higher than the available amount of levels minus one. It has been reduced to that number."))
    amt_spp <- highest_poss_spp

    spp_cand <- 1:highest_poss_spp + 0.5
  }
  else if (amt_spp == highest_poss_spp) {
    spp_cand <- 1:highest_poss_spp + 0.5
  }
  else {
    spp_cand <- sort(sample(highest_poss_spp, amt_spp) + 0.5)
  }

  return(spp_cand)
}