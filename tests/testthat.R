
Sys.setenv("R_TESTS" = "")
library(testthat)
library(klava)

test_check("klava", reporter="summary")

