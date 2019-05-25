##' ---
##' output: md_document
##' ---

#+ echo = FALSE, comment = '.', message = FALSE
library(optimhelp)
library(rbenchmark)
library(dplyr)
library(rlang)
library(assertthat)

foo <- function(x) {
  x <- as.character(x)[2]
  parse_expr(x)
}

foo(~a)


x <- all_log(CL = 1.1, V2 = 3, KA = 4, A = 3, B=4, C=5)

x <- trans(x)
x
untrans(x)
data(exTheoph)

b <- c(a = 1, b = 2, c = 3)
benchmark(
  as.list(x), 
  list2env(as.list(x)), 
  get_pars(x), 
  c(exTheoph,b),
  replications = 1000
)

ti <- tibble(x = 1)
y <- as.list(x)
b <- mutate(ti, D = eval(parse_expr("x+C*A"),y))

eval_tidy(parse_expr("D = C*A"),ti)
mutate(

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

