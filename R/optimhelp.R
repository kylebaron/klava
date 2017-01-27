##' @importFrom methods new
##' @importFrom stats setNames
##' @import methods

.logit <- function(x) {
  log(x/(1-x))
}
.alogit <- function(x) exp(x)/(1+exp(x))
ident <- function(x) x

##' Create a new pars object.
##'
##' @param ... par objects
##' @export
##'
parset <- function(...) {
  x <- new("parset", data=list(...))
  if(any(duplicated(names(x)))) stop("Duplicated parameter names.", call.=FALSE)
  names(x@data) <- names(x)
  return(x)
}

##' Create a list of log-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @param ... passed to constructor
##' @export
##'
log_par <- function(name,value,...) {
  new("logpar", name=name, value=value,...)
}
##' Create a list of logit-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @param ... passed to constructor
##' @export
##'
logit_par <- function(name,value,...) {
  new("logitpar",name=name,value=value,...)
}

##' Create a list of untransformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @param ... passed to constructor
##' @export
##'
ident_par <- function(name,value,...) {
  new("par", name=name, value=value,...)
}

##' Fix some parameters in a set.
##'
##' @param x parset object
##' @param ... unquoted names of paramters to fix
##'
##' @export
fix_par <- function(x,...) {
  tofx <- as.character(match.call()[-c(1,2)])
  if(!all(is.element(tofx,names(x)))) {
    diff <- setdiff(tofx,names(x))
    stop("Name(s) not found: ",paste(diff,collapse=","),call.=FALSE)
  }
  fx <- names(x) %in% tofx

  x@data[fx] <- lapply(x@data[fx], function(y) {
    y@fixed <- TRUE
    y
  })
  x
}
