
significance.test <- function(model, signif = F, round = 6) {
  if (class(model) == "ar") {
    coeff.vect <- model$ar
    var.vect <- diag(model$asy.var.coef)
  } else if (class(model) == "Arima") {
    coeff.vect <- coef(model)[1:length(model$coef)]
    var.vect <- diag(model$var.coef)[1:length(model$coef)]
  } else {
    warning(paste0(
      "Model must be of class AR or ARIMA but is ",
      toupper(class(model)), " !"
    ), immediate. = T)
    return(NA)
  }
  
  values <- coef.p(coeff.vect, var.vect)
  values <- round(values, round)
  
  if (signif) {
    values <- sapply(values, function(v) {
      if (v < 0.001) {
        s <- "***"
      } else if (v < 0.01) {
        s <- "**"
      } else if (v < 0.05) {
        s <- "**"
      } else if (v < 0.1) {
        s <- "."
      } else {
        s <- ""
      }
      return(paste0("(", s, ") ", v))
    })
  }
  
  values <- values[names(values) != "intercept"]
  return(values)
}

ARIMA.order.selection <- function(time.serie, order.max.p = 5, order.max.q = 5,
                                  echo = F, control.maxit = 100) {
  AIC.table <- BIC.table <- array(dim = c(order.max.p, order.max.q))
  for (p in seq(order.max.p)) {
    for (q in seq(order.max.q)) {
      AR <- tryCatch(
        arima(time.serie, order = c(p, 0, q), optim.control = list(maxit=control.maxit)),
        warning = function(w) w
      )
      if (is(AR, "warning")) {
        AIC.table[p, q] <- BIC.table[p, q] <- Inf # Has not converged
      } else {
        AIC.table[p, q] <- Aic.arima(time.serie, AR)
        BIC.table[p, q] <- Bic.arima(time.serie, AR)
      }
    }
  }
  if (echo) { print(AIC.table); print(BIC.table)}
  AIC.best <- which(AIC.table == min(AIC.table), T)
  BIC.best <- which(BIC.table == min(BIC.table), T)
  matrix(c(AIC.best, BIC.best),
         nrow = 2, ncol = 2, byrow = T,
         dimnames = list(c("AIC", "BIC"), c("AR(p)", "MA(q)"))
  )
}

YW.order.selection <- function(time.serie, order.max = 10, echo = F) {
  computed <- sapply(seq(order.max), function(i) {
    aryw <- ar.yw(time.serie, order.max = i, aic = F)
    c(
      Fpe.ar(time.serie, aryw),
      Aic.ar(time.serie, aryw),
      Bic.ar(time.serie, aryw)
    )
  })
  if (echo) print(computed)
  matrix(
    sapply(1:nrow(computed), function(i) which.min(computed[i, ])),
    dimnames = list(c("FPE", "AIC", "BIC"), c("AR(p)"))
  )
}

plot.n.ahead.predictions <- function(time.serie, model, n = 10, before = 48, holtwinters=F) {
  len <- length(time.serie)
  
  if (holtwinters){
    fore = predict(HoltWinters(time.serie, beta = FALSE), n.ahead = n, type="response", prediction.interval=T, level=0.95)
  }else{
    fore <- predict(model, n.ahead = n, type="response")
  }
  
  
  
  before = min(before, length(time.serie))
  ts.freq = frequency(time.serie)
  
  draw = ts(c(tail(time.serie, before), ifelse(isTRUE(holtwinters), fore[1,1], fore$pred[1])), start=end(time.serie)[1]-(before/ts.freq)+1, frequency = ts.freq) 
  
  plot(draw, type="l", main=paste0("Time serie and prediction (", n, "-ahead)", ifelse(holtwinters, "\nHolt-winter's method", "")), ylim=c(min(draw)-0.1*min(draw), max(draw)+0.1*max(draw)),
       xlab = "Time", ylab = "Y", xlim=c(start(draw)[1], end(time.serie)[1]+ceiling(n/ts.freq)+0.1*(n/ts.freq)))
  
  
  if(holtwinters){
    lines(fore[, "fit"], col=2)
    lines(fore[, "upr"], lty="dashed", col=4)
    lines(fore[, "lwr"], lty="dashed", col=4)
  }else{
    lines(fore$pred, col=2)
    lines(fore$pred + 1.96*fore$se, lty="dashed", col=4)
    lines(fore$pred - 1.96*fore$se, lty="dashed", col=4)
  }
  
  legend("topleft", legend = c("Observations", "Prediction", "95% Confidence"), col = c(1, 2, 4), lty = c(1, 1, 2), cex = .8, lwd = 2)
}

plot.last.n.predictions <- function(time.serie, order, seasonal=list(order = c(0L, 0L, 0L), period=NA), n = 10, main="Last n predictions") {
  ts.ahead <- OneAhead(time.serie, order, seasonal)
  
  len <- length(time.serie)
  n <- min(n, 0.8 * len)
  
  plot(time.serie[I(len - n):len], type = "l", main = main, ylab = "Y", xlab = "Time", lwd = 1)
  lines(ts(ts.ahead$tspred[I(len - n):len], start = 0, end = n), col = 2, lty = 2, lwd = 1)
  legend("topleft", legend = c("Observations", "Simulated"), col = c(1, 2), lty = c(1, 2), cex = .8, lwd = 2)
}
