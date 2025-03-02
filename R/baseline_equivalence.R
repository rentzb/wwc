#' Calculate whether matched measures meet WWC standards for baseline equivalence
#'
#' @param psm Output from MatchIt
#' @param plot Logical. Whether to include a plot or not.
#'
#' @return A data frame with columns variable, type of variable, the equivalence measure value, whether baseline equivalence was met, and the baseline equivalence measure used.
#' @export
#'
#' @examples
#' library(MatchIt); library(magrittr); library(ggplot2); data("lalonde", package = "cobalt")
#' m.out1 <- matchit(treat ~ age + educ + race +
#' married + nodegree + re74 + re75,
#' data = lalonde, method = "nearest")
#' baseline_equiv <- baseline_equivalence(psm = m.out1)
baseline_equivalence <- function(psm,plot=TRUE){
  bal_table <- cobalt::bal.tab(psm, un = TRUE,standardize = TRUE,quick = FALSE, binary = "std")
  obs <- bal_table$Observations %>% data.frame() %>% tibble::rownames_to_column("type")
  n_int <- obs %>%
    dplyr::filter(type == "Matched (Unweighted)") %>%
    dplyr::select(Treated) %>% unlist() %>% unname()
  n_comp <- obs %>%
    dplyr::filter(type == "Matched (Unweighted)") %>%
    dplyr::select(Control) %>% unlist() %>% unname()
  balance <- bal_table$Balance %>% data.frame() %>% tibble::rownames_to_column("variable")
  equiv_df <- suppressWarnings(balance %>% dplyr::mutate(equivalence_measure = dplyr::case_when(
    Diff.Adj == 0 ~ 0,
    Type == "Binary" & Diff.Adj != 0~ coxs_index(prevalance_comp = `M.0.Adj`,
                                                 prevalance_int = `M.1.Adj`,
                                                 n_int = n_int,
                                                 n_comp = n_comp),
    Type == "Contin." | Type == "Distance" ~ hedges_g(int_adj_mean = `M.1.Adj`,
                                                      comp_adj_mean = `M.0.Adj`,
                                                      n_int = n_int,
                                                      n_comp = n_int,
                                                      int_unadj_sd = `SD.1.Un`,
                                                      comp_unadj_sd = `SD.0.Un`)
  )) %>%
    dplyr::mutate(baseline_equivalence = dplyr::case_when(
      abs(equivalence_measure) <= 0.25 ~ "baseline equivalent",
      abs(equivalence_measure) > 0.25 ~ "not baseline equivalent",
    )) %>%
    dplyr::mutate(baseline_metric = dplyr::case_when(
      Type == "Binary" ~ "Cox's Index",
      Type == "Contin." | Type == "Distance" ~ "Hedges' G",
    )) %>%
    dplyr::mutate(equivalence_measure = round(equivalence_measure,3)) %>%
    dplyr::select(variable, Type, equivalence_measure,baseline_equivalence,baseline_metric))
  if (plot == TRUE){
    baseline_plot <- equiv_df %>% ggplot(aes(y = variable,
                            x = equivalence_measure,
                            color = baseline_equivalence)) +
      geom_point() + geom_vline(xintercept = -0.25) +
      geom_vline(xintercept = 0.25) + theme_minimal() +
      xlab("Equivalence Measure") + ylab("") +
      labs(color="Baseline Equivalence")
    print(baseline_plot)
    return(equiv_df)
  } else{return(equiv_df)}

}
