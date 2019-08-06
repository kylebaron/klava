#' Objective functions
#' 
#' These functions take in observed and predicted data as well as other
#' data items and return a single objective function value quantifying 
#' how similar the predicted data are to the observed data. 
#' 
#' @param dv observed data vector
#' @param pred model-predicted data vector 
#' @param sigma residual error variance; may be length 1 or length of the 
#' `dv` and `pred` vectors
#' @param logdv if `TRUE`, then observations and predictions will be 
#' log-transformed prior to calculating the objective function value
#' @param ... not used
#' 
#' @details
#' `els`: extended least squares; `ml`: maximum likelihood (normal); 
#' `ols`: ordinary least squares; `wls`: weighted least squares
#' 
#' 
#' @md 
#' @name ofv
#' @rdname ofv
#' @export
els <- function(dv,pred,sigma,logdv=FALSE,...) {
  if(logdv) {
    res <- log(dv)-log(pred) 
  } else {
    res <- dv-pred
  }
  0.5*sum(res^2/sigma + log(sigma), na.rm=TRUE)
}
#' @rdname ofv
#' @export
ml <- function(dv,pred,sigma,logdv=FALSE,...) {
  if(logdv) {
    like <- dnorm(log(dv),log(pred),sqrt(sigma),log=TRUE)
  } else {
    like <- dnorm(dv,pred,sqrt(sigma),log=TRUE)    
  }
  return(-1*sum(like, na.rm=TRUE))
}
#' @rdname ofv
#' @export
ols <- function(dv,pred,...) {
  res <- (dv-pred)
  sum(res^2,na.rm=TRUE)  
}

#' @rdname ofv
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
