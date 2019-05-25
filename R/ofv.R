els_impl <- function(dv,pred,sigma,logdv=FALSE) {
  if(logdv) {
    res <- log(dv)-log(pred) 
  } else {
    res <- dv-pred
  }
  0.5*sum(res^2/sigma + log(sigma), na.rm=TRUE)
}

ml_impl <- function(dv,pred,sigma,logdv=FALSE) {
  if(logdv) {
    like <- dnorm(log(dv),log(pred),sqrt(sigma),log=TRUE)
  } else {
    like <- dnorm(dv,pred,sqrt(sigma),log=TRUE)    
  }
  return(-1*sum(like, na.rm=TRUE))
}

ols_impl <- function(dv,pred,wt=1) {
  res <- (dv-pred)/wt
  sum(res^2,na.rm=TRUE)  
}

#' @export
els <- function(p,theta,data,pred_name,sigma,pred=FALSE,logdv=FALSE,...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  sig <- eval(expr=sigma, c(out,p))
  els_impl(data[["DV"]], out[[pred_name]], sig, logdv = logdv)
}

#' @export
ml <- function(p,theta,data,pred_name,sigma,pred=FALSE,logdv=FALSE,...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  sig <- eval(expr=sigma, c(out,p))
  ml_impl(data[["DV"]], out[[pred_name]],sig, logdv=logdv)
}

#' @export
ols <- function(p,theta,data,pred_name,pred=FALSE,...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  ols_impl(data[["DV"]], out[[pred_name]])
}

#' @export
wls <- function(p,theta,data,pred_name,pred=FALSE,...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  ols_impl(data[["DV"]], out[[pred_name]], wt = data[["DV"]])
}
