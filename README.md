klava: parameter optimzation with mrgsolve
================

## Installation

``` r
devtools:::install_github("kylebaron/klava")
```

## Example

``` r
library(dplyr)
library(mrgsolve)
library(nloptr)
library(ggplot2)
library(klava)
library(rlang)
```

Load an mrgsolve model

``` r
mod <- modlib("pk2")
```

    . Building pk2 ... done.

Grab some data

``` r
data <- readRDS("inst/dat/2cmtA.RDS")

ggplot(data, aes(time,DV)) + geom_point() + theme_bw()
```

![](img/README-unnamed-chunk-6-1.png)<!-- -->

Define a parameter
list

``` r
theta <- all_log(CL = 0.5, V2 = 50, Q = 1.1, V3 = 30, KA = 1.1, sigma=1.1)
```

Fit the model

``` r
fit <- fit_nl(theta, data, mod = mod, pred_name= "CP", cov_step=TRUE,
              pred_initial=TRUE)
```

    . Checking data ...

    . Fitting with els ...done.
    . Generating predictions.
    . Trying cov step ... success.

Result

``` r
fit$tab
```

    . # A tibble: 6 x 5
    .   par   start    final        lb       ub
    .   <chr> <dbl>    <dbl>     <dbl>    <dbl>
    . 1 CL      0.5  0.955    0.900     1.01   
    . 2 V2     50   21.5     19.2      24.0    
    . 3 Q       1.1  1.89     1.10      3.27   
    . 4 V3     30    8.87     7.01     11.2    
    . 5 KA      1.1  1.10     0.941     1.28   
    . 6 sigma   1.1  0.00153  0.000687  0.00340

``` r
plot(fit)
```

![](img/README-unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(fit$data, aes(time,RES)) + geom_point() + 
  geom_hline(yintercept=0) + theme_bw()
```

![](img/README-unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot(fit$data, aes(PRED,DV)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1) + theme_bw()
```

![](img/README-unnamed-chunk-12-1.png)<!-- -->

## Objective functions

Extended Least Squares - ELS

``` r
fit <- fit_nl(theta, data, mod, pred_name= "CP", ofv=els)
```

    . Checking data ...

    . Fitting with els ...done.
    . Generating predictions.

Normal likelihood

``` r
fit <- fit_nl(theta, data, mod, pred_name= "CP", ofv=ml)
```

    . Checking data ...

    . Fitting with ml ...done.
    . Generating predictions.

Ordinary Least Squares - OLS

``` r
fit <- fit_nl(theta, data, mod, pred_name= "CP", ofv=ols)
```

    . Checking data ...

    . Fitting with ols ...done.
    . Generating predictions.

Weighted Least Squares - WLS

``` r
fit <- fit_nl(theta, data, mod, pred_name= "CP", ofv=wls)
```

    . Checking data ...

    . Fitting with wls ...done.
    . Generating predictions.
