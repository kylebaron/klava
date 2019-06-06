klava
================
kylebtwin

## Installation

``` r
devtools:::install_github("kylebtwin/klava")
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
data <- readRDS("inst/data/2cmtA.RDS")

ggplot(data, aes(time,DV)) + geom_point() + theme_bw()
```

![](img/README-unnamed-chunk-5-1.png)<!-- -->

Define a parameter list

``` r
theta <- all_log(CL = 0.1, V2 = 100, Q = 1, V3 = 30, KA = 1, sigma=1)
```

Fit the
model

``` r
fit <- fit_nl(theta, data, pred_name= "CP", cov_step=TRUE, pred_initial=TRUE)
```

    . Checking data ...

    . Fitting with els ...done.
    . Generating predictions.
    . Trying cov step ... Loading required namespace: nlme

    . Warning in sqrt(diag(solve(co$Hessian))): NaNs produced

    . Warning in cov_step(fit, ofv, theta, vdat, pred_name, sigma = sigma):
    . trouble with cov step

Result

``` r
fit$tab
```

    . # A tibble: 6 x 4
    .   par   start    final       se
    .   <chr> <dbl>    <dbl>    <dbl>
    . 1 CL      0.1  0.301   NaN     
    . 2 V2    100   24.7       0.0185
    . 3 Q       1    1.20    NaN     
    . 4 V3     30   49.8     NaN     
    . 5 KA      1    1.35      0.0520
    . 6 sigma   1    0.00485   0.394

``` r
ggplot(fit$data) + 
  geom_line(aes(time,PRED),col="black", lwd=1) +
  geom_line(aes(time,INITIAL), col = "grey", lty=2, lwd=1) + 
  geom_point(aes(time,DV),size=3, col = "firebrick")  + 
  scale_y_log10() + theme_bw()
```

![](img/README-unnamed-chunk-9-1.png)<!-- -->

## Objective functions

ELS

``` r
fit <- fit_nl(theta, data, pred_name= "CP", ofv=els)
```

    . Checking data ...

    . Fitting with els ...done.
    . Generating predictions.

Normal likelihood

``` r
fit <- fit_nl(theta, data, pred_name= "CP", ofv=ml)
```

    . Checking data ...

    . Fitting with ml ...done.
    . Generating predictions.

OLS

``` r
fit <- fit_nl(theta, data, pred_name= "CP", ofv=ols)
```

    . Checking data ...

    . Fitting with ols ...done.
    . Generating predictions.

WLS

``` r
fit <- fit_nl(theta, data, pred_name= "CP", ofv=wls)
```

    . Checking data ...

    . Fitting with wls ...done.
    . Generating predictions.
