ident <- function(x) x
fixed <- ident
logit <- function(x) log(x/(1-x))
unlogit <- function(x) exp(x)/(1+exp(x))
dof <- function(fun,value) fun(value)
mdof <- function(a,b) mapply(dof, a, b)

validatep <- function(...) {
  input <- list(...)
  assert_that(is.list(input))
  assert_that(length(input)==1)
  assert_that(nchar(names(input)) > 0)
  assert_that(is.numeric(input[[1]]))
  list(name = names(input), value = as.numeric(input))
}

#' Create parameter transformation objects
#' 
#' 
#' @param a single named numeric value (see examples)
#' @param fun the transformation-specific constructor function
#' 
#' @examples
#' 
#' as_par(CL = 2)
#' 
#' as_par_fixed(g = 9.8)
#' 
#' @export
as_par <- function(..., fun = log_par) {
  x <- validatep(...)
  fun(x$name, x$value)
}

#' @rdname as_par
#' @export
as_par_logit <- function(...) as_par(..., fun = logit_par)

#' @rdname as_par
#' @export
as_par_fixed <- function(...) as_par(..., fun = fixed_par)

#' @rdname as_par
#' @export
all_log <- function(... ) {
  x <- list(...)
  na <- names(x)
  ans <- vector("list", length(na))
  for(i in seq_along(na)) {
    ans[[i]] <- log_par(na[[i]], x[[i]])  
  }
  do.call(parset,ans)
}

#' @rdname as_par
#' @export
as_par_ident <- function(... ) as_par(..., fun = ident_par)

#' @rdname as_par
#' @export
log_par <- function(name, value, fixed = FALSE) {
  ans <- list(value=value,tr=log,un=exp,name=name,trans = FALSE, fixed = fixed)   
  structure(ans, class="par")
}

#' @rdname as_par
#' @export
logit_par <- function(name, value, fixed = FALSE) {
  ans <- list(value=value,tr=logit,un=unlogit,name=name,trans = FALSE, fixed = fixed)  
  structure(ans, class="par")
}

#' @rdname as_par
#' @export
fixed_par <- function(name, value, fixed=TRUE) {
  ans <- list(value = value, tr = ident, un = ident, name=name, trans=FALSE, fixed = TRUE)
  structure(ans, class="par")
}

#' @rdname as_par
#' @export
ident_par <- function(name,value) {
  ans <- fixed_par(name,value)
  ans[["fixed"]] <- FALSE
  ans
}

#' @rdname as_par
#' @export
new_par <- function(name, value, tr, un, fixed = FALSE, trans = FALSE) {
  ans <- list(value = value, name = name, tr = tr, un = un, fixed = fixed, trans = trans)
  structure(ans,class="par")
}

#' Apply transformations to a parset object
#' 
#' @param x a parset object
#' 
#' @examples
#' x <- quick_par(CL = log(2))
#' 
#' x
#' 
#' trans(x)
#' 
#' untrans(trans(x))
#' 
#' @export
trans <- function(x) {
  assert_that(is.parset(x))
  x[["value"]] <- get_trans(x)
  x[["trans"]] <- TRUE
  x
}

#' @rdname trans
#' @export
untrans <- function(x) {
  assert_that(is.parset(x))
  x[["value"]] <- get_untrans(x)
  x[["trans"]] <- FALSE
  x
}

#' Get values from a parset object
#' 
#' @param x a parset object
#' 
#' @examples
#' x <- quick_par(CL = log(1), V2 = log(20), F1 = logit(0.77))
#' 
#' get_trans(x)
#' 
#' get_untrans(x)
#' 
#' @export
get_trans <- function(x) {
  assert_that(is.parset(x))
  if(x[["trans"]])  return(x[["value"]])  
  return(mdof(x[["tr"]], x[["value"]]))
}

#' @rdname get_trans
#' @export
get_untrans <- function(x) {
  assert_that(is.parset(x))
  if(!x[["trans"]])  return(x[["value"]])  
  return(mdof(x[["un"]], x[["value"]]))
}
#' @rdname get_trans
#' @export 
get_initials <- function(x) {
  setNames(get_trans(x),get_names(x))[which_estimated(x)]
}

#' @rdname get_trans
#' @export
initials <- function(x) {
  get_initials(x)  
}

#' @rdname get_trans
#' @export
get_pars <- function(x) {
  fx <- x[["fixed"]]
  values <- get_untrans(x)
  setNames(values, x[["names"]])
}

get_names <- function(x) {
  x[["names"]]
}

which_fixed <- function(x) {
  x[["fixed"]]  
}

which_estimated <- function(x) {
  !x[["fixed"]]      
}

#' Create a parset object
#' 
#' @param ... par objects
#' @param trans not used
#' 
#' 
#' @export
parset <- function(...) {
  x <- list(...)
  fx <- sapply(x, "[[", "fixed", USE.NAMES=FALSE)
  tr <- lapply(x, "[[", "tr")
  un <- lapply(x, "[[", "un")
  value <- sapply(x, "[[", "value", USE.NAMES=FALSE)
  na <- sapply(x, "[[", "name", USE.NAMES=FALSE)
  if(any(duplicated(na))) stop("duplicate parameter names")
  ans <- list(value = value, tr = tr, un = un, fixed = fx, names = na, 
              trans = FALSE, scale = value)
  structure(ans, class="parset")
}

#' @export
names.parset <- function(x) x[["names"]]

#' Add a par object to a parset object
#' 
#' @param a parset object
#' @param a par object
#' 
#' @export
parset_add <- function(x, nw) {
  m <- length(x[["value"]]) + 1
  x[["value"]][m] <- nw[["value"]]
  x[["names"]][m] <- nw[["name"]]
  if(any(duplicated(x[["names"]]))) {
    stop("duplicated parameter names", call.=FALSE)  
  }
  x[["tr"]][[m]] <- nw[["tr"]]
  x[["un"]][[m]] <- nw[["un"]]
  x[["fixed"]][m] <- nw[["fixed"]]
  x[["scale"]][m] <- nw[["value"]]
  x
}

#' @export
print.parset <- function(x,...) {
  tr <- rep(ifelse(x[["trans"]], "t", "u"), length(x[["value"]]))
  fx <- ifelse(x[["fixed"]], "*", "")
  print(data.frame(name=x[["names"]],value=x[["value"]],tr=tr,fx=fx), row.names=FALSE)
  return(invisible(NULL))
}

#' @export
print.par <- function(x, ...) {
  print(parset(x))
}

#' Graft new values into an existing parset object
#' 
#' @param x a parset object
#' @param y numeric vector of new values
#' 
#' 
#' @export
graft <- function(x, y) {
  x <- trans(x)
  x[["value"]][which_estimated(x)] <- y
  mapply(dof, x[["un"]], x[["value"]])
}

#' @rdname graft
#' @export
graft_par <- function(x,y) {
  setNames(graft(x,y), x[["names"]])
}

#' Quick and easy syntax for constructing parlist 
#' 
#' @param ... named value, with an optional transformation of the value; 
#' valid transformations include `log`, `logit`, `fixed`, `ident`; see
#' examples
#' 
#' @examples
#' 
#' quick_par(CL = log(1), KA = fixed(1.0), F1 = logit(0.8))
#' 
#' @export
quick_par <- function(...) {
  a <-  enexprs(...)
  
  fun <- sapply(lapply(a, as.character), "[[", 1, USE.NAMES=FALSE)
  
  invalid <- !(fun %in% c("log", "logit", "fixed", "ident"))
  if(any(invalid)) {
    which_invalid <- fun[invalid]
    stop("Transformation not recognized: ", paste0(which_invalid, collapse = ','))
  }
  value <- sapply(a, eval, envir=sys.frame(1), USE.NAMES=FALSE)
  antifun <- c(log = "exp", exp = "log", 
               logit = "unlogit", unlogit = "logit", 
               fixed = "ident", ident = "fixed", 
               logp = "exp", logitp = "unlogit")
  fun <- unname(fun)
  afun <- antifun[fun]
  n <- names(a)
  ans <- vector("list", length(value))
  for(i in seq_along(value)) {
    ans[[i]] <- new_par(
      name = n[i], 
      value = value[i], 
      tr = get(fun[i], mode = "function"), 
      un = get(afun[i], mode = "function"), 
      fixed = "fixed"==fun[i]
    )
  }
  x <- do.call(parset, ans)
  x[["trans"]] <- TRUE
  x <- untrans(x)
  x
}

#' @export
coef.parset <- function(object, all = FALSE, ...) {
  ans <- get_pars(object)
  if(all) {
    return(ans)
  }
  return(ans[which_estimated(object)])
}

#' @export
is.parset <- function(x) inherits(x, "parset")

#' @export
is.par <- function(x) inherits(x, "par")

#' @method as.list parset
#' @export
as.list.parset <- function(x,...) {
  ans <- as.list(get_untrans(x))
  setNames(ans, names(x))
}

#' @export
add_sigma <- function(x,value=10) {
  n <- length(value)
  if(n==1) {
    return(parset_add(x,as_par(sigma=value)))
  }
  for(i in seq(n)) {
    na <- paste0("sigma",i)
    x <- parset_add(x,log_par(na,value[i]))
  }
  x
}
