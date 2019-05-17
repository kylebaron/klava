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

#' @export
as_par <- function(..., fun = log_par) {
  x <- validatep(...)
  fun(x$name, x$value)
}

#' @export
as_par_logit <- function(...) as_par(..., fun = logit_par)

#' @export
as_par_fixed <- function(...) as_par(..., fun = fix_par)

#' @export
as_par_ident <- function(... ) as_par(..., fun = ident_par)

#' @export
log_par <- function(name,value, fixed = FALSE) {
  ans <- list(value=value,tr=log,un=exp,name=name,trans = FALSE, fixed = fixed)   
  structure(ans, class="par")
}

#' @export
logit_par <- function(name,value,fixed = FALSE) {
  ans <- list(value=value,tr=logit,un=unlogit,name=name,trans = FALSE, fixed = fixed)  
  structure(ans, class="par")
}

#' @export
fix_par <- function(name,value,fixed=TRUE) {
  ans <- list(value = value, tr = ident, un = ident, name=name, trans=FALSE, fixed = TRUE)
  structure(ans, class="par")
}

#' @export
ident_par <- function(name,value,...) {
  ans <- fix_par(name,value)
  ans[["fixed"]] <- FALSE
  ans
}

#' @export
new_par <- function(name, value, tr, un, fixed = FALSE, trans = FALSE) {
  ans <- list(value = value, name = name, tr = tr, un = un, fixed = fixed, trans = trans)
  structure(ans,class="par")
}

#' @export
trans <- function(x) {
  x[["value"]] <- get_trans(x)
  x[["trans"]] <- TRUE
  x
}

#' @export
untrans <- function(x) {
  x[["value"]] <- get_untrans(x)
  x[["trans"]] <- FALSE
  x
}

#' @export
get_trans <- function(x) {
  if(x[["trans"]])  return(x[["value"]])  
  return(mdof(x[["tr"]], x[["value"]]))
}

#' @export
get_untrans <- function(x) {
  if(!x[["trans"]])  return(x[["value"]])  
  return(mdof(x[["un"]], x[["value"]]))
}

#' @export 
get_initials <- function(x) {
  setNames(get_trans(x),get_names(x))[which_estimated(x)]
}

#' @export
get_pars <- function(x) {
  fx <- x[["fixed"]]
  values <- get_untrans(x)
  setNames(values, x[["names"]])
}

#' @export
get_names <- function(x) {
  x[["names"]]
}

#' @export
which_fixed <- function(x) {
  x[["fixed"]]  
}

#' @export
which_estimated <- function(x) {
  !x[["fixed"]]      
}

#' @export
parlist <- function(..., trans = FALSE) {
  x <- list(...)
  fx <- sapply(x, "[[", "fixed", USE.NAMES=FALSE)
  tr <- lapply(x, "[[", "tr")
  un <- lapply(x, "[[", "un")
  value <- sapply(x, "[[", "value", USE.NAMES=FALSE)
  na <- sapply(x, "[[", "name", USE.NAMES=FALSE)
  structure(list(value = value, tr = tr, un = un, fixed = fx, names = na, trans = trans), class="parlist")
}

#' @export
names.parlist <- function(x) x[["names"]]

#' @export
initials <- function(x) {
  get_initials(x)  
}


#' @export
parlist_add <- function(x, nw) {
  m <- length(x[["value"]]) + 1
  x[["value"]][m] <- nw[["value"]]
  x[["names"]][m] <- nw[["name"]]
  if(any(duplicated(x[["names"]]))) {
    stop("duplicated parameter names", call.=FALSE)  
  }
  x[["tr"]][[m]] <- nw[["tr"]]
  x[["un"]][[m]] <- nw[["un"]]
  x[["fixed"]][m] <- nw[["fixed"]]
  x
}

#' @export
print.parlist <- function(x,...) {
  tr <- rep(ifelse(x[["trans"]], "t", "u"), length(x[["value"]]))
  fx <- ifelse(x[["fixed"]], "*", "")
  print(data.frame(name=x[["names"]],value=x[["value"]],tr=tr,fx=fx), row.names=FALSE)
  return(invisible(NULL))
}

#' @export
print.par <- function(x, ...) {
  print(parlist(x))
}

#' @export
graft <- function(x, nw) {
  x <- trans(x)
  x[["value"]][which_estimated(x)] <- nw
  mapply(optimhelp:::dof, x[["un"]], x[["value"]])
}

#' @export
graft_par <- function(x,nw) {
  setNames(graft(x,nw), x[["names"]])
}

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
  x <- do.call(parlist, ans)
  x[["trans"]] <- TRUE
  x <- untrans(x)
  x
}

#' 
#' @export
coef.parlist <- function(object, all = FALSE, ...) {
  ans <- get_pars(object)
  if(all) {
    return(ans)
  }
  return(ans[which_estimated(object)])
}

is.parlist <- function(x) inherits(x, "parlist")
