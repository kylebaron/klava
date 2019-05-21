##' ---
##' output: md_document
##' ---

#+ echo = FALSE, comment = '.', message = FALSE
library(optimhelp)
library(rbenchmark)
library(dplyr)
library(rlang)
library(assertthat)


x <- rnorm(30, 10, 5)
mu <- mean(x)
sigma <- var(x)

fun <- function(par,x) {
  mu <- par[1]
  -1*sum(dnorm(x, mu,sqrt(sigma), log = TRUE))
}

fun2 <- function(par,x) {
  mu <- par[1]
  0.5*sum(((mu-x)/sigma)^2 + log(sigma))
}

fit <- optim(par = c(5), fn = fun2, x= x)

library(nlme)
he <- fdHess(fit$par, fun=fun2, x = x)
se <- he$Hessian %>% solve %>% diag %>% sqrt
sqrt(var(x)/length(x))


x <- all_log(CL = 1.1, V2 = 3, KA = 4)

x <- trans(x)
x
untrans(x)


x <- quick_par(CL = log(1.1), V = log(20), KA = log(1.6), 
                FOO = fixed(2), eps = logit(0.5))


as.list(x)
as.numeric(x)

trans(x)
untrans(x)
coef(x)
initials(x)
names(x)





x <- parlist(
  as_par(CL = 0.5) , 
  as_par(V2 = 30), 
  as_par_ident(g = 9.8), 
  as_par_fixed(a = 3), 
  as_par_fixed(foo = 0), 
  as_par(sigma1 = 1), 
  as_par(sigma2 = 2)
)

a <- initials(x) + 2

graft_par(x,a)

