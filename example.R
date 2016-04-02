library(optimhelp)



x <- runif(300,10,300)
y <- 0.9*x/(100+x)*exp(rnorm(length(x),0,sqrt(0.05)))
data <- data.frame(x=x,y=y)

emax <- logit_par("emax", 0.9)
ec50 <- log_par("ec50", 100)
p <- new_pars(emax,ec50)
p


pred <- function(est,p, x) {
  est <- as.list(untrans(p,est))
  yhat <- est$emax*x/(x+est$ec50)
  sqres <- (y-yhat)^2
  return(sum(sqres))
}



fit <-optim(par=trans(p),fn=pred,p=p,x=x)

untrans(p,fit$par)


##' # Fit with `nls`
prednls <- function(p, x, emax,ec50) {
  est <- as.list(untrans(p,c(emax=emax,ec50=ec50)))
  yhat <- est$emax*x/(x+est$ec50)
  return(yhat)
}

fit <- nls(y~prednls(p=p,x=x,emax,ec50),data=data, start=trans(p))

fit

untrans(p,coef(fit))















