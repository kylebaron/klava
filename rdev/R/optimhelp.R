.logit <- function(x) {
  log(x/(1-x))
}
.alogit <- function(x) exp(x)/(1+exp(x))

##' Create a new pars object.
##'
##' @param ... lists with parameter information
##' @export
new_pars <- function(...) {
  x <- list(...)
  structure(x,class="pars")
}
##' Get parameter names.
##'
##' @param x pars object
##' @export
labels <- function(x) {
  lapply(x, function(y) y$name) %>% unlist
}
##' Get parameter values.
##'
##' @param x pars object
##' @export
##'
values <- function(x) {
  a <- lapply(x, function(y) y$value) %>% unlist
  names(a) <- labels(x)
  a
}
##' Transform parameter values.
##'
##' @param p pars object
##' @param x optinal values vector of values with same length as p to transform
##' @export
##'
trans <- function(p,x) {
  if(missing(x)) x <- values(p)
  stopifnot(length(x)==length(p))
  b <- mapply(x,p, FUN=function(a,b) {
    b$to(a)
  })
  names(b) <- labels(p)
  b
}
##' Untransform parameter values.
##'
##' @param p pars object
##' @param x optional values vector with same length as p to untransform
untrans <- function(p,x) {
  if(missing(x)) x <- values(p)
  stopifnot(length(x)==length(p))
  b <- mapply(x,p, FUN=function(a,b) {
    b$from(a)
  })
  names(b) <- labels(p)
  b
}

##' Create a list of log-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @export
##'
log_par <- function(name,value) {
  list(name=name,value=value, to=log, from=exp)
}
##' Create a list of logit-transformed parameters.
##'
##' @param name the parameter name
##' @param value the parameter value
##' @export
##'
logit_par <- function(name,value) {
  list(name=name,value=value,to=.logit,from=.alogit)
}
##' Print the parameter list.
##' @param x pars object
##'
##' @export
##'
print.pars <- function(x) {
  print(data.frame(name=labels(x), value=values(x)))
}


