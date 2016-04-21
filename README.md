optimhelp
=========

``` r
library(optimhelp)
```

    . 
    . Attaching package: 'optimhelp'

    . The following object is masked from 'package:base':
    . 
    .     transform

So far, this is a parameter management system. I can name and set initial values for parameters in a model. In this example, I can also specify a transformation for that parameter: the value is transformed to a different scale for estimation and it can be transformed back when either getting a prediction or after the optimization is finished.

In this example, both `CL` and `VC` are estimated as log-transformed values.

``` r
cl <- log_par("CL", 1.2)
vc <- log_par("VC", 22.3)
p <- new_pars(cl,vc)
```

After making a `pars` object, we can look at it

``` r
p
```

    .  name value transf tr fx
    .    CL   1.2    log  u   
    .    VC  22.3    log  u

or transform the starting values to the estimation scale

``` r
start.values <- initials(p)
start.values
```

    .        CL        VC 
    . 0.1823216 3.1045867

grafting back in gets us untransformed (by default)

``` r
graft(p,start.values)
```

    .  name value transf tr fx
    .    CL   1.2    log  u   
    .    VC  22.3    log  u

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

We will restrict emax to be between 0 and 1 for now. Also sending a fixed parameter through for fun

``` r
emax <- logit_par("emax", 0.6)
ec50 <- log_par("ec50", 60)
fx <- ident_par("yak", 1234, fixed=TRUE)
p <- new_pars(emax,ec50,fx)
p
```

    .  name  value transf tr fx
    .  emax    0.6  logit  u   
    .  ec50   60.0    log  u   
    .   yak 1234.0  ident  u  *

Fit with `optim`
----------------

``` r
pred <- function(est,p, x) {
  est <- as.list(graft(p,est))
  yhat <- est$emax*x/(x+est$ec50)
  sqres <- (y-yhat)^2
  return(sum(sqres))
}



fit <-optim(par=initials(p),fn=pred,p=p,x=x)

untrans(graft(p,fit$par))
```

    .  name        value transf tr fx
    .  emax    0.8801455  logit  u   
    .  ec50   91.2968742    log  u   
    .   yak 1234.0000000  ident  u  *

Fit with `nls`
--------------

``` r
prednls <- function(p, x, emax,ec50) {
  est <- as.list(untrans(graft(p,c(emax=emax,ec50=ec50))))
  yhat <- est$emax*x/(x+est$ec50)
  return(yhat)
}

fit <- nls(y~prednls(p=p,x=x,emax,ec50),data=data, start=initials(p))

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
est <- graft(p,coef(fit))

coef(est)
```

    .       emax       ec50 
    .  0.8801505 91.2936089

``` r
coef(fit)
```

    .     emax     ec50 
    . 1.993856 4.514081
