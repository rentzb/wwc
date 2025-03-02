#' Calculate Hedges' g for continuous measures
#'
#' @param int_adj_mean The intervention group's adjusted mean.
#' @param comp_adj_mean The comparison group's adjustment mean.
#' @param n_int The number of observations in the intervention group.
#' @param n_comp The number of observations in the comparison group.
#' @param int_unadj_sd The unadjusted standard deviation of the intervention group.
#' @param comp_unadj_sd The unadjusted standard deviation of the comparison group.
#'
#' @return The Hedges' g value.
#' @export
#'
#' @examples
#' g <- hedges_g(int_adj_mean = 1.0,
#'       comp_adj_mean = 0.75,
#'       n_int = 100,
#'       n_comp = 150,
#'       int_unadj_sd = 0.1,
#'       comp_unadj_sd = 0.2)
hedges_g <- function(int_adj_mean = 1.0,
                     comp_adj_mean = 0.75,
                     n_int = 100,
                     n_comp = 150,
                     int_unadj_sd = 0.1,
                     comp_unadj_sd = 0.2) {
  omega = 1 - (3/(4*(n_int + n_comp)-9)) # small sample size correction
  numerator = (omega * (int_adj_mean - comp_adj_mean))
  denominator = sqrt(((n_int - 1)* int_unadj_sd^2 + (n_comp - 1)* comp_unadj_sd^2)/(n_int + n_comp - 2))
  g = numerator/denominator
  return(g)
}
