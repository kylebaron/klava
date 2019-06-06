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

mod <- modlib("pk2")
data <- readRDS("inst/data/2cmtA.RDS")

theta <- all_log(CL = 1, V2 = 30, Q = 1, V3 = 30, KA = 1,sigma=1)
theta <- quick_par(CL = log(1), V2 = log(20), 
                   Q = log(2), V3 = log(30), KA = log(1), 
                   sigma = log(1), foo = fixed(20))


klava:::apply_untrans(theta,initials(theta))



fit <- fit_nl(theta, data, pred_name= "CP", cov_step=TRUE, 
              optimizer = "neldermead")

ggplot(fit$data) + 
  geom_line(aes(time,PRED)) +
  geom_point(aes(time,DV))  + 
  scale_y_log10()

