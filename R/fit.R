els_impl <- function(dv,pred,sigma, logdv = FALSE) {
  if(logdv) {
    sum(((log(dv)-log(pred))^2)/sigma + log(sigma), na.rm=TRUE)    
  } else { 
    sum(((dv-pred)^2)/sigma + log(sigma), na.rm=TRUE)  
  }
}

ml_impl <- function(dv,pred,sigma, logdv = FALSE) {
  if(logdv) {
    like <- dnorm(log(dv),log(pred),sqrt(sigma),log=TRUE)
  } else {
    like <- dnorm(dv,pred,sqrt(sigma),log=TRUE)    
  }
  return(-1*sum(like, na.rm=TRUE))
}

ols_impl <- function(dv,pred,wt=1) {
  res <- (dv-pred)/wt
  sum(res^2,na.rm=TRUE)  
}

#' @export
els <- function(p, theta, data, pred_name, pred = FALSE, logdv=FALSE) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  els_impl(data[["DV"]], out[[pred_name]],p[["sigma"]], logdv = logdv)
}

#' @export
ml <- function(p, theta, data, pred_name, pred = FALSE, logdv=FALSE, ...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  ml_impl(data[["DV"]], out[[pred_name]],p[["sigma"]], logdv=logdv)
}

#' @export
ols <- function(p, theta, data, pred_name, pred = FALSE,...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  ols_impl(data[["DV"]], out[[pred_name]])
}

#' @export
wls <- function(p, theta, data, pred_name, pred = FALSE,...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  ols_impl(data[["DV"]], out[[pred_name]], wt = data[["DV"]])
}

tran_upper <- c("CMT", "II", "ADDL", "EVID", "AMT", "RATE", "MDV", "TIME")
lctran <- function(data) {
  n <- names(data)
  infrom <- is.element(n,c("MDV",tran_upper))
  haslower <- is.element(tolower(n),n)
  change <- infrom & !haslower
  if(sum(change) > 0) names(data)[change] <- tolower(n[change])
  data
}

#' @export
nl_optr <- function(theta, data, 
                    optimizer = c("newuoa", "neldermead", "optim"),
                    pred_name = "CP", ofv = els, logdv = FALSE,
                    pred_initial = FALSE, cov_step = FALSE,
                    algorithm = "NLOPT_LN_NEWUOA",...) {
  
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
    ini, fn=ofv, theta = theta, data = vdat, pred_name=pred_name, logdv = logdv, ...
  ) 
  message("done.\nGenerating predicttions.")
  pr <- ofv(fit$par, theta=theta, data = vdat, pred_name = pred_name, pred=TRUE)
  data[["PRED"]] <- pr[[pred_name]]
  data[["RES"]] <- data[["DV"]] - data[["PRED"]]
  if(pred_initial) {
    pr <- ofv(ini, theta = theta,   data = vdat, pred_name = pred_name, pred = TRUE)
    data[["INITIAL"]] <- pr[[pred_name]]
  }
  fit$data <- data
  coe <- coef(theta)
  fit$tab <- tibble(par = names(coe), start =ini, final = fit$par)
  if(cov_step) {
    message("Trying cov step ... ", appendLF=FALSE)
    assert_that(requireNamespace("nlme"))
    co <- try(
      nlme::fdHess(fit$par, ofv, theta = theta, data = vdat, pred_name = pred_name)
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
  }
  
  fit
}