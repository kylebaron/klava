##' ---
##' output:
##'   md_document:
##'     variant: markdown_github
##' ---
library(optimhelp)

x <- runif(300,10,300)
y <- 0.9*x/(100+x)*exp(rnorm(length(x),0,sqrt(0.05)))
data <- data.frame(x=x,y=y)

emax <- logit_par("emax", 0.9)
ec50 <- log_par("ec50", 100)
a <- ident_par("b", 2.2, fixed=TRUE)

emax2 <- logit_par("emax2", 0.9)
ec502 <- log_par("ec502", 100)
a2 <- ident_par("b2", 2.2, fixed=TRUE)

emax3 <- logit_par("emax3", 0.9)
ec503 <- log_par("ec503", 100)
a3 <- ident_par("b3", 2.2, fixed=TRUE)


p <- new_pars(emax,ec50,a)




benchmark(
graft(p,as.numeric(p)),
graft(p,as.numeric(p)[1]),replications=1000)



as.list(p)

coef(p)

pred <- function(est,p, x) {
  est <- as.list(graft(p,est))
  yhat <- est$emax*x/(x+est$ec50)
  sqres <- (y-yhat)^2
  return(sum(sqres))
}


fit <-optim(par=initials(p),fn=pred,p=p,x=x)

est <- graft(p,fit$par)
est





##' # Fit with `nls`
prednls <- function(p, x, emax,ec50) {
  est <- as.list(graft(p,c(emax=emax,ec50=ec50)))
  yhat <- est$emax*x/(x+est$ec50)
  return(yhat)
}


fit <- nls(y~prednls(p=p,x=x,emax,ec50),data=data, start=initials(p))

fit

e <- graft(p,coef(fit))

e

coef(e)

coef(e,all=TRUE)



