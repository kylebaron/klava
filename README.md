optimhelp
=========

``` r
library(optimhelp)
```

So far, this is a **parameter management system**. I can name and set initial values for parameters in a model. I can also specify a transformation for each parameter: the value is transformed to a different scale for estimation and it can be transformed back when either getting a prediction or after the optimization is finished.

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

or see what the transformed values look like

``` r
trans(p)
```

    .  name     value transf tr fx
    .    CL 0.1823216    log  t   
    .    VC 3.1045867    log  t

or generate initial values on the estimation scale

``` r
start.values <- initials(p)
start.values
```

    .        CL        VC 
    . 0.1823216 3.1045867

Grafting back into the `parlist` object gets us untransformed (by default)

``` r
graft(p,start.values + 0.5)
```

    .  name     value transf tr fx
    .    CL  1.978466    log  u   
    .    VC 36.766484    log  u

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

In the `pred` function below 1. Graft the estimated proposed by the optimzer back into the `parlist` object \* By default, grafting untransforms all values in the `parlist` 1. Coerce to `list` so we can use to generate predictions

``` r
pred <- function(est,p, x) {
  est <- as.list(graft(p,est))
  yhat <- est$emax*x/(x+est$ec50)
  sqres <- (y-yhat)^2
  return(sum(sqres))
}



fit <-optim(par=initials(p),fn=pred,p=p,x=x)
```

After the fitting is done, graft the final estimates back into the `parlist` and take a look.

``` r
est <- graft(p,fit$par)

est
```

    .  name        value transf tr fx
    .  emax    0.8801455  logit  u   
    .  ec50   91.2968742    log  u   
    .   yak 1234.0000000  ident  u  *

A `coef` method will give just the non-fixed values

``` r
coef(est)
```

    .       emax       ec50 
    .  0.8801455 91.2968742

Otherwise, we can get everything like this

``` r
as.numeric(est)
```

    .         emax         ec50          yak 
    .    0.8801455   91.2968742 1234.0000000

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

``` r
est
```

    .  name        value transf tr fx
    .  emax    0.8801505  logit  u   
    .  ec50   91.2936089    log  u   
    .   yak 1234.0000000  ident  u  *
