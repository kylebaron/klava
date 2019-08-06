##' ---
##' output: md_document
##' ---

#+ echo = FALSE, comment = '.', message = FALSE
library(rbenchmark)
library(microbenchmark)
library(dplyr)
library(rlang)
library(assertthat)
library(mrgsolve)
library(nloptr)
library(ggplot2)
library(klava)
mod <- modlib("pk2")

data <- readRDS("inst/dat/2cmtA.RDS")

theta <- all_log(CL = 0.5, V2 = 50, Q = 3.1, V3 = 30, KA = 1.1,
                 sigma=1.1)

theta2 <- quick_par(CL = log(2), 
                    V2 = log(50), 
                    Q = log(3.1), 
                    V3 = log(30), 
                    KA = log(1.1), 
                    sigma = log(1.1))

fit <- fit_nl(theta, data, mod, pred_name= "CP", cov_step=TRUE)

fit

coef(fit, all = TRUE)
plot(fit)

