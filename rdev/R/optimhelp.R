.logit <- function(x) {
  log(x/(1-x))
}
.alogit <- function(x) exp(x)/(1+exp(x))
ident <- function(x) x

##' Create a new pars object.
##'
##' @param ... lists with parameter information
##' @export
new_pars <- function(...) {
  x <- new("parset", data=list(...))
  names(x@data) <- names(x)
  return(x)
}

##' Create a list of log-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @export
##'
log_par <- function(name,value,...) {
  new("logpar", name=name, value=value,fixed=FALSE)
}
##' Create a list of logit-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @export
##'
logit_par <- function(name,value,fixed=FALSE,...) {
  new("logitpar",name=name,value=value,fixed=FALSE)
}
##' Create a list of untransformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @export
##'
ident_par <- function(name,value,...) {
  new("par", name=name, value=value,...)
}



