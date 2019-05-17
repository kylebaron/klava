##' @importFrom methods new
##' @importFrom stats setNames
##' @import methods
##' @importFrom rlang enexprs

.logit <- function(x) {
  log(x/(1-x))
}
.alogit <- function(x) exp(x)/(1+exp(x))
ident <- function(x) x

