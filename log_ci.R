log.ci <- function(surv.object, alpha = 0.05, events.only = FALSE) {
  
  #- Extract necessary values from survsurv.object object
  surv.est  <- surv.object$surv
  sigma     <- surv.object$std.err #This is sigma, not the SE of S(t)
  surv.time <- surv.object$time
  
  #- Get limits element wise
  lims <- data.frame(time = surv.object$time, surv = surv.est,  sigma = surv.object$std.err)
  
  lims$lower <- surv.est^( 1/( exp( qnorm(1 - alpha / 2) * sigma  / log(surv.est) ) ) )
  
  lims$upper <- surv.est^( ( exp( qnorm(1 - alpha / 2) * sigma  / log(surv.est) ) ) )
  
  if (events.only == TRUE) {
    return(round(lims[which(surv.object$n.censor == 0), ], 3)) 
  } else {
    return(round(lims, 3))
  }
}
