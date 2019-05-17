.logit <- function(x) {
  log(x/(1-x))
}

.alogit <- function(x) exp(x)/(1+exp(x))

valu <- function(x) {
  x@value
}

setClass("par", slots=c(name="character", value="numeric", fixed="logical",trans="logical"),
         prototype=list(trans=FALSE,fixed=FALSE))
setClass("logpar", contains="par")
setClass("logitpar", contains="par")
setClass("fixpar", contains="par")



##' Name required parameters in a parset.
##' 
##' @param x a parset object
##' @param what name of parameter to require
##' 
##' @export
##' 
require_par <- function(x,what) {
  all(what %in% names(x))
}
  

##' Par object class.
##'
##' @param object par object
##' @param x par object
##' @param value the initial, untransformed value of the parameter
##' @param trans the parameter transformation
##' @param ... passed along
##'
##' @rdname par
##' @name par
NULL


##' @export
##' @rdname par
setMethod("update", "par", function(object,value,trans=TRUE,...) {
  object@value <- value
  object@trans <- trans
  return(object)
})



##' Graft new estimates into a \code{parset} object.
##'
##' @param x a parset object
##' @param y named list or vector of estimates to graft into the parlist object
##' @param untrans logical indicating whether to untransform the object before returning
##' @param ... not used
##'
##' @export
graft__ <-  function(x,y,untrans=TRUE,...) {

  if(!is.parset(x)) {
    stop("x must be a parset object")
  }

  y <- unlist(y)
  if(is.null(names(y))) {
    names(y) <- names(which(estimated(x)))
  }

  if(!all(names(y) %in% names(x))) {
    stop("Graft failed; could not find some parameters up for grafting.", call.=FALSE)
  }

  for(nn in names(y)) {
     x@data[[nn]] <- update(x@data[[nn]], value=y[nn],trans=TRUE)
  }

  if(untrans) x <- untrans(x)

  return(x)
}
# 
# 
# setMethod("coef", "parset", function(object,all=FALSE,...) {
#   a <- as.numeric(object)
#   if(all) return(a)
#   a[estimated(object)]
# })
# 
# 

