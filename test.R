##' ---
##' output: md_document
##' ---

#+ echo = FALSE, comment = '.', message = FALSE
library(rbenchmark)
library(dplyr)
library(rlang)
library(assertthat)
library(mrgsolve)
library(nloptr)
library(ggplot2)

mod <- modlib("pk1")

data <- readRDS("inst/dat/2cmtA.RDS")

theta <- all_log(CL = 1, V2 = 30, Q = 1, V3 = 30, KA = 1,sigma=1)

theta <- quick_par(CL = log(1), V2 = log(20), 
                   Q = log(0.02), V3 = log(3), KA = log(1), 
                   sigma = log(1.1))

theta <- all_log(CL = 1.1, V = 100, KA = 1.1, sigma=1.1)

fit <- fit_nl(theta, data, mod, pred_name= "CP", cov_step=TRUE, 
              optimizer = "newuoa", pred_initial=TRUE)



ggplot(fit$data) + 
  geom_line(aes(time,PRED)) +
  geom_line(aes(time,INITIAL)) +
  geom_point(aes(time,DV))  + 
  scale_y_log10()


wf_fit <- function(p,mod,pars,data,dv_col,pred=FALSE) {
  p <- lapply(p,exp)
  names(p) <- pars
  mod <- param(mod,p)
  out <- mrgsim_q(mod,data,output="df")
  if(pred) return(out)
  res <- data[["DV"]]-out[[dv_col]]
  if("sigma2" %in% pars) {
    return(0.5 * sum(res^2/p[["sigma2"]] + log(p[["sigma2"]]),na.rm=TRUE))
  } else {
    return(sum(res^2,na.rm=TRUE))
  }
}

wf_fit_predict <- function(fit,mod,pars,data,dv_col) {
  pr <- wf_fit(fit$par,mod,pars,data,dv_col,pred=TRUE)
  data[["PRED"]] <- pr[[dv_col]]
  data
}

mod <- modlib("pk2")
theta <- log(c(CL = 10, V2 = 30,Q=1,V3=30, KA = 1, sigma2=1))
dat <- list(data = data, pars = names(theta), dv_col = "CP", mod =mod)

fit <- newuoa(theta, wf_fit, mod = mod, pars = names(theta),data=data, 
              dv_col = "CP", control=list(maxeval=2000))

data <- wf_pred(fit,mod,names(theta),data=data,dv_col="CP")

ggplot(data = data, aes(x = time)) + 
  geom_point(aes(y = DV)) + 
  geom_line(aes(y = PRED))




