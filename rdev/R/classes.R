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

##' @export
setGeneric("tform", function(x,...) standardGeneric("tform"))
setMethod("tform", "par", function(x,...) return("ident"))
setMethod("tform", "logpar", function(x,...) return("log"))
setMethod("tform", "logitpar", function(x,...) return("logit"))



##' @export
setMethod("update", "par", function(object,value,trans=TRUE...) {
  object@value <- value
  object@trans <- trans
  return(object)
})

##' @export
setMethod("names", "par", function(x) x@name)

##' @export
setGeneric("trans", function(x,...) standardGeneric("trans"))
setMethod("trans", "par", function(x,...) return(x))
setMethod("trans", "logpar", function(x,...) {
  if(x@trans) return(x)
  x@value <- log(x@value)
  x@trans <- TRUE
  return(x)
})
setMethod("trans", "logitpar", function(x,...) {
  if(x@trans) return(x)
  x@value <- .logit(x@value)
  x@trans <- TRUE
  return(x)

})

##' @export
setGeneric("untrans", function(x,...) standardGeneric("untrans"))
setMethod("untrans", "par", function(x,...) return(x))
setMethod("untrans", "logpar", function(x,...) {
  if(!x@trans) return(x)
  x@value <- exp(x@value)
  x@trans <- FALSE
  return(x)

})
setMethod("untrans", "logitpar", function(x,...) {
  if(!x@trans) return(x)
  x@value <- .alogit(x@value)
  x@trans <- FALSE
  return(x)
})

setClass("parset", slots=c(data="list"))
##' @export
setMethod("trans", "parset", function(x,...) {
    x@data <- lapply(x@data, trans)
    return(x)
})

##' @export
setMethod("untrans", "parset", function(x,...) {
  x@data <- lapply(x@data, untrans)
  return(x)
})

##' @export
setMethod("as.numeric", "parset", function(x, ...) {
    setNames(as.numeric(sapply(x@data, valu)), names(x))
})
##' @export
setMethod("as.list", "parset", function(x, ...) {
  setNames(as.list(sapply(x@data, valu)), names(x))
})


##' @export
setMethod("names", "parset", function(x) {
    unlist(lapply(x@data, names))
})

setMethod("tform", "parset", function(x,...) {
    as.character(sapply(x@data, tform))
})

##' @export
setGeneric("initials", function(x,...) standardGeneric("initials"))
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

##' @export
setMethod("show", "parset", function(object) {
    n <- names(object)
    v <- as.numeric(object)
    tr <- ifelse(get_trans(object), "t", "u")
    transf <- tform(object)
    fx <- ifelse(estimated(object), "", "*")
    print(data.frame(name=n,value=v,transf = transf,tr=tr,fx=fx), row.names=FALSE)

})

##' @export
graft <-  function(x,what,untrans=TRUE,...) {
  what <-unlist(what)

  if(!all(names(what) %in% names(x))) {
    stop("Graft failed; could not find some parameters up for grafting.", call.=FALSE)
  }

  for(nn in names(what)) {
     x@data[[nn]] <- update(x@data[[nn]], value=what[nn],trans=TRUE)
  }

  if(untrans) x <- untrans(x)

  return(x)
}

##' @export
setMethod("coef", "parset", function(object,all=FALSE,...) {
  a <- as.numeric(object)
  if(all) return(a)
  a[estimated(object)]
})


