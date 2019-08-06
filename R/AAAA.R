#' @importFrom methods new
#' @importFrom stats setNames dnorm coef
#' @import methods
#' @importFrom rlang enexprs parse_expr exprs
#' @importFrom ggplot2 aes scale_y_log10 geom_point geom_line theme_bw ggplot
#' @importFrom dplyr tibble left_join mutate if_else .data
#' @importFrom assertthat assert_that
#' @importFrom mrgsolve mrgsim_q param valid_data_set zero_re simargs
#' @importMethodsFrom mrgsolve as.data.frame
#' @importFrom nlme fdHess
#' 
NULL

globalVariables(c("time", "DV", "PRED", "INITIAL"))