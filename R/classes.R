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
setClass("parset", slots=c(data="list"))

is.parset <- function(x) any(class(x)=="parset")
is.par <- function(x) any(class(x) %in% c("logpar", "logitpar", "fixpar", "par"))

##' Parset object class.
##'
##' @param object par object
##' @param x par object
##' @param name parameter name
##'
##' @name parset
##' @rdname parset
NULL

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


##' Get the name of the transformation function.
##'
##' @param x the object
##' @param ... not used
##'
##' @export
setGeneric("tform", function(x,...) standardGeneric("tform"))
##' @rdname tform
setMethod("tform", "par", function(x,...) return("ident"))
##' @rdname tform
setMethod("tform", "logpar", function(x,...) return("log"))
##' @rdname tform
setMethod("tform", "logitpar", function(x,...) return("logit"))
##' @rdname tform
setMethod("tform", "parset", function(x,...) {
  as.character(sapply(x@data, tform))
})

##' @export
##' @rdname par
setMethod("update", "par", function(object,value,trans=TRUE,...) {
  object@value <- value
  object@trans <- trans
  return(object)
})

##' @export
##' @rdname par
setMethod("names", "par", function(x) x@name)

##' Transform transformed parameters in a parset object.
##'
##' @param x a par or parset object
##' @param ... not used
##'
##' @export
setGeneric("trans", function(x,...) standardGeneric("trans"))
##' @rdname trans
##' @export
setMethod("trans", "par", function(x,...) return(x))
##' @rdname trans
##' @export
setMethod("trans", "logpar", function(x,...) {
  if(x@trans | x@fixed) return(x)
  x@value <- log(x@value)
  x@trans <- TRUE
  return(x)
})
##' @rdname trans
##' @export
setMethod("trans", "logitpar", function(x,...) {
  if(x@trans | x@fixed) return(x)
  x@value <- .logit(x@value)
  x@trans <- TRUE
  return(x)

})
##' @rdname trans
##' @export
setMethod("trans", "parset", function(x,...) {
  x@data <- lapply(x@data, trans)
  return(x)
})

##' Untransform parameters in a parset object.
##'
##' @param x a par or parset object
##' @param ... not used
##'
##' @export
setGeneric("untrans", function(x,...) standardGeneric("untrans"))
##' @rdname untrans
##' @export
setMethod("untrans", "par", function(x,...) return(x))
##' @rdname untrans
##' @export
setMethod("untrans", "logpar", function(x,...) {
  if(!x@trans | x@fixed) return(x)
  x@value <- exp(x@value)
  x@trans <- FALSE
  return(x)

})
##' @rdname untrans
##' @export
setMethod("untrans", "logitpar", function(x,...) {
  if(!x@trans | x@fixed) return(x)
  x@value <- .alogit(x@value)
  x@trans <- FALSE
  return(x)
})
##' @rdname untrans
##' @export
setMethod("untrans", "parset", function(x,...) {
  x@data <- lapply(x@data, untrans)
  return(x)
})

##' @rdname parset
##' @export
setMethod("as.numeric", "parset", function(x, ...) {
    setNames(as.numeric(sapply(x@data, valu)), names(x))
})
##' @rdname parset
##' @export
setMethod("as.list", "parset", function(x, ...) {
  setNames(as.list(sapply(x@data, valu)), names(x))
})


##' @export
##' @rdname parset
setMethod("names", "parset", function(x) {
    unlist(lapply(x@data, names))
})


##' Generate initial estimates from a parset.
##'
##' @param x a parset object
##' @param ... not used
##'
##' @export
setGeneric("initials", function(x,...) standardGeneric("initials"))
##' @rdname initials
##' @export
setMethod("initials", "parset", function(x,...) {
    a <- as.numeric(trans(x))
    b <- estimated(x)
    a[b]
})

estimated <- function(x) {
    sapply(x@data, function(y) !y@fixed)
}

get_trans <- function(x) {
  sapply(x@data, function(y) y@trans)

}

get_fixed <- function(x) {
  fx <- sapply(x@data,function(y) y@fixed)
  if(!any(fx)) return(list())
  as.list(x)[fx]
}

##' @rdname parset
##' @export
setMethod("show", "parset", function(object) {
    n <- names(object)
    v <- as.numeric(object)
    tr <- ifelse(get_trans(object), "t", "u")
    transf <- tform(object)
    fx <- ifelse(estimated(object), "", "*")
    print(data.frame(name=n,value=v,transf = transf,tr=tr,fx=fx), row.names=FALSE)

})

##' @rdname par
##' @export
setMethod("show", "par", function(object) {
  print(parset(object))
})

##' Graft new estimates into a \code{parset} object.
##'
##' @param x a parset object
##' @param y named list or vector of estimates to graft into the parlist object
##' @param untrans logical indicating whether to untransform the object before returning
##' @param ... not used
##'
##' @export
graft <-  function(x,y,untrans=TRUE,...) {

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

##' Get estimated (non-fixed) values from a parset object.
##' @param object the parset object
##' @param all return non-fixed values too
##' @param ... not used
##'
##' @seealso \code{\link{coeff}}
##'
##' @export
setMethod("coef", "parset", function(object,all=FALSE,...) {
  a <- as.numeric(object)
  if(all) return(a)
  a[estimated(object)]
})

##' Get estimated and fixed values from a parset object.
##'
##' @param x a parset object
##'
##' @export
coeff <- function(x) {
  coef(x,all=TRUE)
}

##' @rdname parset
##' @export
setMethod("$", "parset", function(x,name){
  unlist(valu(x@data[[name]]))
})


