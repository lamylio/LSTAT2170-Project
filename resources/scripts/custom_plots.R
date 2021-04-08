# Override the stats S3 method in order to customize the title
plot.decompose.ts <- function(time.series, type = c("additive", "multiplicative"), ...) {
  x <- decompose(time.series, type)
  xx <- x$x
  if (is.null(xx)) {
    xx <- with(x, if (type == "additive") {
      random + trend + seasonal
    } else {
      random * trend * seasonal
    })
  }
  plot(cbind(Observed = xx, Trend = x$trend, Seasonal = x$seasonal, Random = x$random), ...)
}


plot.variance.ts <- function(time.series, title="", std = T, ...) {
  ts.attr <- attributes(gas)$tsp
  ts.start <- ts.attr[1]
  ts.end <- ts.attr[2]
  ts.freq <- ts.attr[3]
  
  variances <- sapply(seq(from = 1, to = abs(ts.end - ts.start)), function(i) var(time.series[ts.freq * (i - 1) + (1:ts.freq)]))
  if (std) {
    variances <- sqrt(variances)
  }
  plot(variances, type = "b", xlab = "Year", ylab = ifelse(std, "Standard deviation", "Variance"), main=title, ...)
  # abline(h = quantile(variances, .25), col = 4, lty = 2)
  # abline(h = quantile(variances, .75), col = 4, lty = 2)
  # legend("topleft", legend = c("1st quantile", "3rd quantile"), col = 4, lty = 2, cex = .8)
}

plot.superposed.ts = function(time.series, title="Superposed", ...){
  ts.attr <- attributes(gas)$tsp
  ts.start <- ts.attr[1]
  ts.end <- ts.attr[2]
  ts.freq <- ts.attr[3]
  
  plot(time.series[1:ts.freq],
       type = "l", main = title,
       ylim = c(min(time.series), max(time.series)), ...
  )
  
  for (i in 2:abs(ts.end - ts.start)) {
    lines(time.series[ts.freq * (i - 1) + (1:ts.freq)], col = i, lty = ifelse(i > 8, 2, 1), lwd = ifelse(i > 8, 2, 1))
  }
  
  legend("topleft", legend = ts.start:(ts.end - 1), lty = c(rep(1, 8), rep(2, abs(ts.end - ts.start) - 8)), col = 1:abs(ts.end - ts.start), cex = 0.5, bg = "white")
}
