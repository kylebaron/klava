optimhelp
=========

``` r
library(optimhelp)
cl <- log_par("CL", 1.2)
vc <- log_par("VC", 22.3)
p <- new_pars(cl,vc)
```

``` r
p
```

    .    name value
    . CL   CL   1.2
    . VC   VC  22.3

``` r
start.values <- trans(p)
start.values
```

    .        CL        VC 
    . 0.1823216 3.1045867

``` r
untrans(p,start.values)
```

    .   CL   VC 
    .  1.2 22.3

Examples
========

Simulate some data
------------------

``` r
set.seed(292)

x <- runif(300,10,300)
y <- 0.9*x/(100+x)*exp(rnorm(length(x),0,sqrt(0.05)))
data <- data.frame(x=x,y=y)
head(data)
```

    .           x         y
    . 1 122.01323 0.6512473
    . 2  83.96901 0.3334132
    . 3 151.43117 0.4865623
    . 4 109.99034 0.6853547
    . 5 200.64816 0.5596619
    . 6 259.40643 0.4126136

Specify parameters
------------------

We will restrict emax to be between 0 and 1 for now.

``` r
emax <- logit_par("emax", 0.6)
ec50 <- log_par("ec50", 60)
p <- new_pars(emax,ec50)
p
```

    .      name value
    . emax emax   0.6
    . ec50 ec50  60.0

Fit with `optim`
----------------

``` r
pred <- function(est,p, x) {
  est <- as.list(untrans(p,est))
  yhat <- est$emax*x/(x+est$ec50)
  sqres <- (y-yhat)^2
  return(sum(sqres))
}



fit <-optim(par=trans(p),fn=pred,p=p,x=x)

untrans(p,fit$par)
```

    .       emax       ec50 
    .  0.8801455 91.2968742

Fit with `nls`
--------------

``` r
prednls <- function(p, x, emax,ec50) {
  est <- as.list(untrans(p,c(emax=emax,ec50=ec50)))
  yhat <- est$emax*x/(x+est$ec50)
  return(yhat)
}

fit <- nls(y~prednls(p=p,x=x,emax,ec50),data=data, start=trans(p))

fit
```

    . Nonlinear regression model
    .   model: y ~ prednls(p = p, x = x, emax, ec50)
    .    data: data
    .  emax  ec50 
    . 1.994 4.514 
    .  residual sum-of-squares: 4.214
    . 
    . Number of iterations to convergence: 4 
    . Achieved convergence tolerance: 2.305e-06

``` r
untrans(p,coef(fit))
```

    .       emax       ec50 
    .  0.8801505 91.2936089
