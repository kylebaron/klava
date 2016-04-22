.logit <- function(x) {
  log(x/(1-x))
}
.alogit <- function(x) exp(x)/(1+exp(x))
ident <- function(x) x

##' Create a new pars object.
##'
##' @param ... par objects
##' @export
new_pars <- function(...) {
  x <- new("parset", data=list(...))
  if(any(duplicated(names(x)))) stop("Duplicated parameter names.", call.=FALSE)
  names(x@data) <- names(x)
  return(x)
}

##' Create a list of log-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @param fixed logical indicating whether the parameter should be fixed
##' @param ... not used
##' @export
##'
log_par <- function(name,value,...) {
  new("logpar", name=name, value=value,fixed=FALSE)
}
##' Create a list of logit-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @param fixed logical indicating whether the parameter should be fixed
##' @param ...
##' @export
##'
logit_par <- function(name,value,fixed=FALSE,...) {
  new("logitpar",name=name,value=value,fixed=FALSE)
}
##' Create a list of untransformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @param fixed logical indicating whether the parameter should be fixed
##' @export
##'
ident_par <- function(name,value,fixed=FALSE,...) {
  new("par", name=name, value=value,fixed=fixed,...)
}



