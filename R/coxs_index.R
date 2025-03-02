#' Calculate Cox's Index for binary measures
#'
#' @param prevalance_int The prevalence of the outcome for the intervention group.
#' @param prevalance_comp The prevalence of the outcome for the comparison group.
#' @param n_int The number of observations in the intervention group.
#' @param n_comp The number of observations in the comparison group.
#'
#' @return The Cox's Index value.
#' @export
#'
#' @examples
#' i <- coxs_index(prevalance_int = 0.4, prevalance_comp = 0.3, n_int = 100, n_comp = 110)
coxs_index <- function(prevalance_int = 0.4, prevalance_comp = 0.3, n_int = 100, n_comp = 110) {
  omega = 1 - (3/(4*(n_int + n_comp)-9))
  d = omega * (log(prevalance_int/(1-prevalance_int)) - log(prevalance_comp/(1-prevalance_comp)))/1.65
  return(d)
}
