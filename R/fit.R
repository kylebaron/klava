
tran_upper <- c("CMT", "II", "ADDL", "EVID", "AMT", "RATE", "MDV", "TIME")

lctran <- function(data) {
  n <- names(data)
  infrom <- is.element(n,c("MDV",tran_upper))
  haslower <- is.element(tolower(n),n)
  change <- infrom & !haslower
  if(sum(change) > 0) names(data)[change] <- tolower(n[change])
  data
}

#' Fit a model to data using functions from nloptr
#' 
#' @param theta a parset object
#' @param data a data set in data.frame format
#' @param mod a mrgsolve model object
#' @param optimizer used to search the parameter space
#' @param pred_name name in simulated output representing `PRED`
#' @param ofv objective function function
#' @param logdv logical; if `TRUE`, then the dependent variable is 
#' log-transformed during `ofv` calculation
#' @param sigma a one-sided formula containing an expression for the residual
#' error variance
#' @param pred_initial if `TRUE`, a column is added to the output called
#' `INITIAL` which is a model prediction at the initial parameter estimates
#' @param cov_step if `TRUE`, then standard errors are derived using 
#' the Hessian
#' @param ... passed to `ofv`
#' 
#' @export
fit_nl <- function(theta, data, mod,
                   optimizer = c("newuoa", "neldermead"),
                   pred_name = "CP", ofv = els, logdv = FALSE, sigma = ~sigma,
                   pred_initial = FALSE, cov_step = FALSE, ...) {
  
  mod <- zero_re(mod)
  mod <- simargs(mod,clear=TRUE)
  sigma <- as.character(sigma)[2]
  sigma <- parse_expr(sigma)
  
  optimizer <- match.arg(optimizer)
  
  opt <- get(optimizer, mode = "function")
  
  fn <- deparse(substitute(ofv))
  if(fn %in% c("ols", "wls")) {
    if(cov_step) {
      warning("disabling cov_step with ofv ", fn,call.=FALSE)
      cov_step <- FALSE
    }
  }
  ini <- initials(theta)
  if(any(ini==0)) {
    stop("initial values on transformed scale cannot be zero.", call.=FALSE)
  }
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
  data <- mutate(data, DV = ifelse(.data[["mdv"]]==1, NA_real_, .data[["DV"]]))
  vdat <- as.data.frame(valid_data_set(data,mod))
  message("Fitting with ", fn, " ...", appendLF=FALSE)
  fit <- opt(
    ini, fn=simlike, theta = theta, data = vdat, mod=mod, 
    pred_name=pred_name, logdv = logdv, sigma = sigma, ofv=ofv, ...
  ) 
  message("done.\nGenerating predictions.")
  pr <- simlike(fit$par,theta=theta,data=vdat,mod=mod,
                pred_name=pred_name,pred=TRUE)
  data[["PRED"]] <- pr[[pred_name]]
  data[["RES"]] <- data[["DV"]] - data[["PRED"]]
  if(pred_initial) {
    pr <- simlike(ini,theta=theta,data=vdat,mod=mod, ofv=ofv,
                  pred_name=pred_name,sigma=sigma,pred=TRUE)
    data[["INITIAL"]] <- pr[[pred_name]]
  }
  fit$data <- data
  start <- get_untrans(theta)
  na <- get_names(theta)
  fit$untab <- tibble(
    par = na, 
    start = get_trans(theta), 
    final = get_trans(graft_in(theta,fit$par))
  )
  fit$tab <- tibble(par = na, start = start, final = graft_par(theta,fit$par))
  if(cov_step) {
    fit <- cov_step(fit,ofv,theta,vdat,mod,pred_name,sigma=sigma)
    fit$untab[["lb"]] <- fit$untab[["final"]] - 1.96*fit$untab[["se"]]
    fit$untab[["ub"]] <- fit$untab[["final"]] + 1.96*fit$untab[["se"]]
    fit$tab[["lb"]] <- apply_untrans(theta,fit$untab[["lb"]])
    fit$tab[["ub"]] <- apply_untrans(theta,fit$untab[["ub"]])
  }
  structure(fit, class=c("klv_fit", "list"))
}

#' @export
plot.klv_fit <- function(x, log=TRUE) {
  
  
  p <- 
    ggplot(x$data) + 
    geom_line(aes(time,PRED),lwd=0.8) +
    geom_point(aes(time,DV), col = "blue3", size = 3) + 
    theme_bw()
  if(log) p <- p + scale_y_log10()
  
  if("INITIAL" %in% names(x$data)) {
    p <- p + geom_line(aes(time,INITIAL),lty=2,col="darkgrey",lwd=0.8)
  }

  p
}


cov_step <- function(fit,ofv,theta,vdat,mod,pred_name,sigma) {
  message("Trying cov step ... ", appendLF=FALSE)
  assert_that(requireNamespace("nlme"))
  co <- try(
    nlme::fdHess(fit$par, simlike, theta = theta,data = vdat, ofv=ofv,
                 mod = mod,pred_name = pred_name, sigma = sigma)
  )
  if(inherits(co, "try-error")) {
    warning("not successful.")  
  } else {
    se <- try(sqrt(diag(solve(co$Hessian))))
    if(inherits(se,"try-error")) {
      warning("not successful.")
      return(fit)
    }
    if(all(!is.nan(se))) {
      message("success.")  
    } else {
      warning("trouble with cov step")  
    }
    sedf <- tibble(se = se, par = names(coef(theta)))
    fit$untab <- left_join(fit$untab,sedf,by="par")
  }
  return(fit)
}

