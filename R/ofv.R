#' @export
els <- function(dv,pred,sigma,logdv=FALSE,...) {
  if(logdv) {
    res <- log(dv)-log(pred) 
  } else {
    res <- dv-pred
  }
  0.5*sum(res^2/sigma + log(sigma), na.rm=TRUE)
}
#' @export
ml <- function(dv,pred,sigma,logdv=FALSE,...) {
  if(logdv) {
    like <- dnorm(log(dv),log(pred),sqrt(sigma),log=TRUE)
  } else {
    like <- dnorm(dv,pred,sqrt(sigma),log=TRUE)    
  }
  return(-1*sum(like, na.rm=TRUE))
}
#' @export
ols <- function(dv,pred,...) {
  res <- (dv-pred)
  sum(res^2,na.rm=TRUE)  
}

#' @export
wls <- function(dv,pred,...) {
  res <- (dv-pred)/dv
  sum(res^2,na.rm=TRUE)  
}

simlike <- function(p,theta,data,mod,pred_name,sigma,pred=FALSE,logdv=FALSE,
                    ofv, ...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  sig <- eval(expr=sigma, c(out,p))  
  ofv(data[["DV"]], out[[pred_name]], sig, logdv = logdv,...)
}
