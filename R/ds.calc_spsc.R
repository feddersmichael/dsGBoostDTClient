
#' Calculate the split score and choose the best one
#'
#' @param split_sums The histogram sums for all split bins.
#' @param spp_cand The split-point candidates for which the bins were
#' calculated.
#' @param reg_par The regularisation parameters.
#' @param cont_NA Vector which numeric feature has additional NA values.
#'
#' @return The split with the best split score.
ds.calc_spsc <- function(split_sums, spp_cand, reg_par, cont_NA) {

  lambda <- reg_par[[1]]
  gamma_reg <- reg_par[[2]]

  # Now we can calculate the split score for all possibilities.
  # From all split scores we can then choose the best split.
  opt_sp_per_leaf <- data.frame(sp_sc = numeric(), feature = numeric(),
                                split_val = numeric(), cont_NA = numeric(),
                                weight_l = numeric(), weight_r = numeric())

  for (i in seq_along(split_sums)) {

    sums <- split_sums[[i]]
    
    if (!is.null(reg_par[["first_leaf"]])) {
      prev_sc <- 0
    } else if ((sums[["compl"]][["hess"]] + lambda) == 0) {
      opt_sp_per_leaf[i, ] <- list(0, "", 0, 0, 0, 0)
      next
    } else {
      prev_sc <- sums[["compl"]][["grad"]]^2 / (sums[["compl"]][["hess"]] + lambda)
    }

    # We write it saving based to check correctness first
    grad <- sums[["grad"]]
    hess <- sums[["hess"]]
    denom_check_l <- 0
    denom_check_r <- 0
    
    split_cont_NA <- 0
    split_feature <- ""
    split_pt <- 0
    spsc <- prev_sc + 2 * gamma_reg
    weight_l <- 0
    weight_r <- 0

    for (feature in names(spp_cand)) {

      if (cont_NA[[i]][feature]) {

        for (j in seq_len(nrow(grad[[feature]]))) {

          denom_check_l <- hess[[feature]]$sum_L_NA[[j]] + lambda
          denom_check_r <- hess[[feature]]$sum_R[[j]] + lambda
          if (denom_check_l != 0 && denom_check_r != 0) {
            cur_weight_l <- grad[[feature]]$sum_L_NA[[j]] / denom_check_l
            cur_weight_r <- grad[[feature]]$sum_R[[j]] / denom_check_r
            left_split_NA <- grad[[feature]]$sum_L_NA[[j]] * cur_weight_l
            right_split <- grad[[feature]]$sum_R[[j]] * cur_weight_r
            cur_spsc <- left_split_NA + right_split
            if (cur_spsc > spsc) {
              spsc <- cur_spsc
              split_feature <- feature
              split_cont_NA <- 1
              split_pt <- spp_cand[[feature]][[j]]
              weight_l <- -cur_weight_l
              weight_r <- -cur_weight_r
            }
          }

          denom_check_l <- hess[[feature]]$sum_L[[j]] + lambda
          denom_check_r <- hess[[feature]]$sum_R_NA[[j]] + lambda
          if (denom_check_l != 0 && denom_check_r != 0) {
            cur_weight_l <- grad[[feature]]$sum_L[[j]] / denom_check_l
            cur_weight_r <- grad[[feature]]$sum_R_NA[[j]] / denom_check_r
            left_split <- grad[[feature]]$sum_L[[j]] * cur_weight_l
            right_split_NA <- grad[[feature]]$sum_R_NA[[j]] * cur_weight_r
            cur_spsc <- left_split + right_split_NA
            if (cur_spsc > spsc) {
              spsc <- cur_spsc
              split_feature <- feature
              split_cont_NA <- 2
              split_pt <- spp_cand[[feature]][[j]]
              weight_l <- -cur_weight_l
              weight_r <- -cur_weight_r
            }
          }
        }
      } else {

        for (j in seq_len(nrow(grad[[feature]]))) {

          denom_check_l <- hess[[feature]]$sum_L[[j]] + lambda
          denom_check_r <- hess[[feature]]$sum_R[[j]] + lambda
          if (denom_check_l != 0 && denom_check_r != 0) {
            cur_weight_l <- grad[[feature]]$sum_L[[j]] / denom_check_l
            cur_weight_r <- grad[[feature]]$sum_R[[j]] / denom_check_r
            left_split <- grad[[feature]]$sum_L[[j]] * cur_weight_l
            right_split <- grad[[feature]]$sum_R[[j]] * cur_weight_r
            cur_spsc <- left_split + right_split
            if (cur_spsc > spsc) {
              spsc <- cur_spsc
              split_feature <- feature
              split_cont_NA <- 0
              split_pt <- spp_cand[[feature]][[j]]
              weight_l <- -cur_weight_l
              weight_r <- -cur_weight_r
            }
          }
        }
      }
    }

    spsc <- (spsc - prev_sc) / 2 - gamma_reg
    best_split <- list(spsc, split_feature, split_pt, split_cont_NA, weight_l,
                       weight_r)
    opt_sp_per_leaf[i, ] <- best_split
  }

  # Finally we return our choice.
  return(opt_sp_per_leaf)
}