els_impl <- function(dv,pred,sigma, logdv = FALSE) {
  if(logdv) {
    sum(((log(dv)-log(pred))^2)/sigma + log(sigma), na.rm=TRUE)    
  } else { 
    sum(((dv-pred)^2)/sigma + log(sigma), na.rm=TRUE)  
  }
}

ml_impl <- function(dv,pred,sigma, log = FALSE) {
  -1*sum(dnorm(dv,pred,sqrt(sigma), log = TRUE),na.rm=TRUE)
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
ml <- function(p, theta, data, pred_name, pred = FALSE,...) {
  p <- graft_par(theta,p)
  mod <- param(mod, p)
  out <- mrgsim_q(mod, data, output="df")
  if(pred) return(out)
  ml_impl(data[["DV"]], out[[pred_name]],p[["sigma"]])
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



#' @export
do_fit <- function(theta, data, xtol_rel=1E-6, maxeval=10000,
                   pred_name = "CP", ofv = els, logdv = FALSE,
                   pred_theta = FALSE,
                   algorithm = "NLOPT_LN_NEWUOA",...) {
  fn <- deparse(substitute(ofv))
  message("Fitting with ", fn)
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
  message("Fitting...")
  fit <- newuoa(
    x0=ini, fn=ofv, theta = theta, data = vdat, pred_name=pred_name, logdv = logdv, ...
  ) 
  message("Generating predicted values ...")
  pr <- ofv(fit$par, theta=theta, data = vdat, pred_name = pred_name, pred=TRUE)
  data[["PRED"]] <- pr[[pred_name]]
  data[["RES"]] <- data[["DV"]] - data[["PRED"]]
  if(pred_theta) {
    pr <- ofv(ini, theta = theta,   data = vdat, pred_name = pred_name, pred = TRUE)
    data[["INITIAL"]] <- pr[[pred_name]]
  }
  fit$data <- data
  fit
}
