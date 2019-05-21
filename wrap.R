library(mrgsolve)
library(nloptr)
library(dplyr)
library(ggplot2)
library(assertthat)
library(optimhelp)
library(purrr)
theme_set(theme_bw())
mod <- modlib("popex") %>% param(TVV = 15) #%>% zero_re(omega)
e <- ev(amt = 100, ii = 24, total = 10)
e <- ev_rep(e, 1:41)
des1 <- tgrid(0,24,4, add = c(0.1,2,6))
des2 <- tgrid(0,24,6) + 192
des3 <- tgrid(0,216,48)
des <- c(des1,des2,des3)

set.seed(11010)
mod <- smat(mod, matrix(0.255))
out <- mrgsim_d(mod, e, tgrid  = des, carry_out = "cmt,evid,ii,addl,amt", recsort=3)
#plot(out, type = 'p')

df <- select(out, ID,time,evid,cmt,ii,addl,amt,DV) %>% filter(!(evid==0 & DV==0))
df <- mutate(df, mdv = evid) %>% setNames(.,toupper(names(.)))

theta <- quick_par(TVCL = log(2.5), TVV = log(30), TVKA = log(1))
thetas <- parset_add(theta,as_par(sigma = 10))
mod <- zero_re(mod)
fit <- nl_optr(
  thetas, df, pred_name = "IPRED", 
  optimizer="newuoa", pred_initial=TRUE,
  logdv = FALSE, ofv = ml, cov_step=TRUE
)
el <- nl_optr(
  thetas, df, pred_name = "IPRED", 
  optimizer="newuoa", pred_initial=TRUE,
  logdv = TRUE, ofv = els, cov_step=TRUE
)

fit$tab
el$tab


fit <- newuoa(x0 = initials(thetas), 
              theta = thetas,
              data = df, 
              pred_name = "IPRED", 
              logdv = TRUE, 
              fn=els)






fit$tab$se/el$tab$se




fit$tab

tofit <- fit$data

tofit

ggplot(data = tofit) + 
  geom_point(aes(time, DV,  group=ID), size = 3) + 
  geom_line(aes(time, PRED, group=ID), lwd=2, col='red4') + 
  geom_line(aes(time, INITIAL, group=ID), lwd=2, col='blue4', lty =2) + 
  scale_y_log10()

