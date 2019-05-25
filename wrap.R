library(mrgsolve)
library(nloptr)
library(dplyr)
library(ggplot2)
library(assertthat)
library(optimhelp)
library(purrr)
library(rlang)

theme_set(theme_bw())
mod <- modlib("popex") %>% param(TVV = 15) #%>% zero_re(omega)
e <- ev(amt = 100, ii = 24, total = 1)
e <- ev_rep(e, 1:1)
des1 <- tgrid(0,48,4, add = c(0.1,2,6))
des2 <- tgrid(0,24,6) + 192
des3 <- tgrid(0,216,48)
des <- c(des1)

set.seed(11010)
mod <- smat(mod, matrix(0.155))
out <- mrgsim_d(mod, e, tgrid  = des, carry_out = "cmt,evid,ii,addl,amt", recsort=3)
#plot(out, type = 'p')

df <- select(out, ID,time,evid,cmt,ii,addl,amt,DV) %>% filter(!(evid==0 & DV==0))
df <- mutate(df, mdv = evid) %>% setNames(.,toupper(names(.)))

thetas <- quick_par(
  TVCL = log(1), TVV = log(100), TVKA = log(1)
) %>% add_sigma(10)

mod <- zero_re(mod)

err <- function(sigma, ipred) {
  sqrt(sigma)
}

el <- nl_optr(
  thetas, df, pred_name = "IPRED", 
  optimizer="newuoa", pred_initial = TRUE,
  ofv = ml, cov_step=TRUE,  logdv=TRUE,
  sigma = ~sigma
)

el$tab

tofit <- el$data

ggplot(data = tofit) + 
  geom_point(aes(time, DV,  group=ID), size = 3) + 
  geom_line(aes(time, PRED, group=ID), lwd=2, col='red4') + 
  geom_line(aes(time, INITIAL, group=ID), lwd=2, col='blue4', lty =2) + 
  scale_y_log10()


