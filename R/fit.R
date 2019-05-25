
tran_upper <- c("CMT", "II", "ADDL", "EVID", "AMT", "RATE", "MDV", "TIME")

lctran <- function(data) {
  n <- names(data)
  infrom <- is.element(n,c("MDV",tran_upper))
  haslower <- is.element(tolower(n),n)
  change <- infrom & !haslower
  if(sum(change) > 0) names(data)[change] <- tolower(n[change])
  data
}

#' Fit a model to data
#' 
#' @param theta a parset object
#' @param data a data set in data.frame format
#' @param optimizer used to search the parameter space
#' @param pred_name name in simulated output representing `PRED`
#' @param ofv objective function function
#' @param logdv logical; if `TRUE`, then the dependent variable is 
#' log-transformed during `ofv` calculation
#' @param cov_step if `TRUE`, then standard errors are derived using 
#' the Hessian
#' @param ... not currently used
#' 
#' @export
nl_optr <- function(theta, data, 
                    optimizer = c("newuoa", "neldermead", "optim"),
                    pred_name = "CP", ofv = els, logdv = FALSE,
                    sigma = ~sigma, 
                    pred_initial = FALSE, cov_step = FALSE, ...) {
  
  sigma <- as.character(sigma)[2]
  sigma <- parse_expr(sigma)
  
  optimizer <- match.arg(optimizer)
  
  opt <- get(optimizer, mode = "function")
  
  fn <- deparse(substitute(ofv))
  if(fn %in% c("ols", "wls")) cov_step <- FALSE
  ini <- initials(theta)
  message("Checking data ...")
  data <- lctran(data)
  
  evid_col <- which(names(data)=="evid")[1]
  assert_that(!is.na(evid_col), msg="evid column is required")
  mdv_col <- which(names(data) %in% c("MDV","mdv"))[1]
  assert_that(exists("DV", data), msg="DV column is required")
  assert_that(exists("time", data), msg="time/TIME column is required")
  if(is.na(mdv_col)) {
    message("Adding mdv column ...")
    data[["mdv"]] <- as.integer(data[[evid_col]] !=0)  
  }
  data <- mutate(data, DV = if_else(mdv==1, NA_real_, DV))
  vdat <- as.data.frame(valid_data_set(data,mod))
  message("Fitting with ", fn, " ...", appendLF=FALSE)
  fit <- opt(
    ini, fn=ofv, theta = theta, data = vdat, pred_name=pred_name, logdv = logdv,
    sigma = sigma, ...
  ) 
  message("done.\nGenerating predicttions.")
  pr <- ofv(fit$par, theta=theta, data = vdat, pred_name = pred_name, pred=TRUE)
  data[["PRED"]] <- pr[[pred_name]]
  data[["RES"]] <- data[["DV"]] - data[["PRED"]]
  if(pred_initial) {
    pr <- ofv(ini, theta = theta,   data = vdat, pred_name = pred_name, sigma=sigma, pred = TRUE)
    data[["INITIAL"]] <- pr[[pred_name]]
  }
  fit$data <- data
  coe <- coef(theta)
  fit$tab <- tibble(par=names(coe), start=ini, final=fit$par)
  if(cov_step) {
    fit <- cov_step(fit, ofv, theta, vdat, pred_name, sigma = sigma) 
  }
  fit
}

cov_step <- function(fit, ofv, theta, vdat, pred_name,sigma) {
  message("Trying cov step ... ", appendLF=FALSE)
  assert_that(requireNamespace("nlme"))
  co <- try(
    nlme::fdHess(fit$par, ofv, theta = theta, data = vdat, pred_name = pred_name, 
                 sigma = sigma)
  )
  if(inherits(co, "try-error")) {
    warning("not successful.")  
  } else {
    se <- sqrt(diag(solve(co$Hessian)))
    if(all(!is.nan(se))) {
      message("success.")  
    } else {
      warning("trouble with cov step")  
    }
    fit$se <- se
    fit$tab <- mutate(fit$tab, se = se)
  }
  return(fit)
}

