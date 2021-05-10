# ---------------------------------------
# Override the stats S3 method in order to customize the title
plot.decompose.ts <- function(time.serie, type = c("additive", "multiplicative"), main="", ...) {
  x <- decompose(time.serie, type)
  xx <- x$x
  if (is.null(xx)) {
    xx <- with(x, if (type == "additive") {
      random + trend + seasonal
    } else {
      random * trend * seasonal
    })
  }
  plot(cbind(Observed = xx, Trend = x$trend, Seasonal = x$seasonal, Random = x$random), main=main, ...)
}

# ---------------------------------------
plot.variance.ts <- function(time.serie, title = "", std = T, ...) {
  ts.attr <- attributes(gas)$tsp
  ts.start <- ts.attr[1]
  ts.end <- ts.attr[2]
  ts.freq <- ts.attr[3]
  
  variances <- sapply(seq(from = 1, to = abs(ts.end - ts.start)), function(i) var(time.serie[ts.freq * (i - 1) + (1:ts.freq)]))
  if (std) {
    variances <- sqrt(variances)
  }
  plot(variances, type = "b", xlab = "Year", ylab = ifelse(std, "Standard deviation", "Variance"), main = title, ...)
  abline(h = quantile(variances, .25), col = 4, lty = 2)
  abline(h = quantile(variances, .75), col = 4, lty = 2)
  legend("topleft", legend = c("1st quantile", "3rd quantile"), col = 4, lty = 2, cex = .8)
}

# ---------------------------------------
plot.superposed.ts <- function(time.serie, title = "Superposed", dashed_thick_from = c(8, 8), ...) {
  ts.attr <- attributes(gas)$tsp
  ts.start <- ts.attr[1]
  ts.end <- ts.attr[2]
  ts.freq <- ts.attr[3]
  
  colors <- rainbow(abs(ts.end - ts.start))
  
  plot(time.serie[1:ts.freq],
       type = "l", main = title,
       ylim = c(min(time.serie), max(time.serie)), ...
  )
  
  for (i in 2:abs(ts.end - ts.start)) {
    lines(time.serie[ts.freq * (i - 1) + (1:ts.freq)], col = colors[i - 1], lty = ifelse(i > dashed_thick_from[1], 2, 1), lwd = ifelse(i > dashed_thick_from[2], 2, 1))
  }
  
  legend("topleft", legend = ts.start:(ts.end-1), lty = c(rep(1, dashed_thick_from[1]), rep(2, abs(ts.end - ts.start) - dashed_thick_from[1])), col = c(1, colors), cex = 0.5, bg = "white")
}

# ---------------------------------------
plot.acf.pacf = function(time.serie, lag.max = 50, simplify=T, linked_by_line=T, titles=c("ACF, PACF"), ...){
  
  # Retrieve the ACF/PACF
  ts.acf = acf(x=time.serie, lag=lag.max, plot = F)
  ts.pacf = pacf(x=time.serie, lag=lag.max, plot=F)
  
  if(simplify){
    # Only keep the rounded lags
    # Improve the visualization
    ts.seq = seq(from=frequency(time.serie), to=lag.max, by=frequency(time.serie))
    
    ts.acf$acf = array(ts.acf$acf[c(1,ts.seq+1)], dim=c(length(ts.seq), 1, 1))
    ts.acf$lag = array(ts.acf$lag[c(1,ts.seq+1)], dim=c(length(ts.seq), 1, 1))
    
    ts.pacf$acf = array(ts.pacf$acf[c(1,ts.seq)], dim=c(length(ts.seq), 1, 1))
    ts.pacf$lag = array(ts.pacf$lag[c(1, ts.seq)], dim=c(length(ts.seq), 1, 1))
  }
  # Plot the modified ones
  # Add lines to improve further the visualization
  
  plot(ts.acf, ylim=c(min(ts.acf$acf)-0.2, min(max(ts.acf$acf+0.3), 1)), main=titles[1], lwd=2, ...)
  if(linked_by_line){lines(ts.acf$lag, ts.acf$acf, type="b", col=rgb(0,0,0,.7))}
  
  plot(ts.pacf, ylim=c(min(ts.pacf$acf)-0.2, min(max(ts.pacf$acf+0.3), 1)), main=titles[2], lwd=2, ...)
  if(linked_by_line){lines(ts.pacf$lag, ts.pacf$acf, type="b", col=rgb(0,0,0,.7))}
}


plot.ljungbox = function(residuals, lag.max = 10, df=0, title="Ljung-Box test of residuals"){
  values = sapply(1:lag.max, function(i) Box.test(residuals, type="Ljung-Box", lag=i, fitdf = df)$p.value)
  plot(values, ylim=c(0,1), xlab="lag", ylab="p-values", type="b", main=title)
  abline(h=0.05, col="blue", lty="dashed")
}

# =======================================

plot.n.ahead.predictions <- function(time.serie, model, n = 10, before = 48, holtwinters=F, HW.alpha=NULL, HW.beta=NULL, HW.gamma=NULL, ...) {
  if (holtwinters){
    fore = predict(HoltWinters(time.serie, alpha=HW.alpha, beta = HW.beta, gamma = HW.gamma), n.ahead = n, type="response", prediction.interval=T, level=0.95)
  }else{
    fore <- predict(model, n.ahead = n, type="response")
  }
  
  before = min(before, length(time.serie))
  ts.freq = frequency(time.serie)
  
  original = ts(tail(time.serie, before), end=end(time.serie), frequency=ts.freq)
  link = ts(c(tail(time.serie, 1), ifelse(isTRUE(holtwinters), fore[1,1], fore$pred[1])), start=end(time.serie), frequency=ts.freq)
  
  plot(original, type="l", main=paste0("Time serie and prediction (", n, "-ahead)", ifelse(holtwinters, "\nHolt-winter's method", "")), 
       ylim=c(min(original)-0.2*min(original), max(original)+0.2*max(original)), 
       xlim=c(start(original)[1], end(time.serie)[1]+(end(time.serie)[2]/ts.freq)+round(n/ts.freq)), ...)
  lines(link, col=2)
  
  if(holtwinters){
    lines(fore[, "fit"], col=2)
    lines(fore[, "upr"], lty="dashed", col=4)
    lines(fore[, "lwr"], lty="dashed", col=4)
  }else{
    lines(fore$pred, col=2)
    lines(fore$pred + 1.96*fore$se, lty="dashed", col=4)
    lines(fore$pred - 1.96*fore$se, lty="dashed", col=4)
  }
  
  legend("topleft", legend = c("Observations", "Prediction", "95% Confidence"), col = c(1, 2, 4), lty = c(1, 1, 2), cex = .7, lwd = 2)
  return(fore)
}
