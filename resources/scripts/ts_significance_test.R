significance.test <- function(model, signif = F) {
  
  if (is(model, "ar")) {
    coeff.vect <- model$ar
    var.vect <- diag(model$asy.var.coef)
  } else if (is(model, "Arima")) {
    coeff.vect <- coef(model)[1:length(model$coef)]
    var.vect <- diag(model$var.coef)[1:length(model$coef)]
  } else {
    warning(paste0(
      "Model must be of class AR or ARIMA but is ", toupper(class(model)), " !"
    ), immediate. = T)
    return(NA)
  }

  coef.p <- function(coeff.vect, var.vect) {
    sigma.vect <- sqrt(var.vect)
    sapply(1:length(sigma.vect), function(i) 2 * pnorm(abs(coeff.vect[i]) / sigma.vect[i], lower.tail = F))
  }

  values <- coef.p(coeff.vect, var.vect)

  if (signif) {
    values <- sapply(values, function(v) {
      if (v <= 0.001) {
        s <- "***"
      } else if (v <= 0.01) {
        s <- "**"
      } else if (v <= 0.05) {
        s <- "**"
      } else if (v <= 0.1) {
        s <- "."
      } else {
        s <- "x"
      }
      return(paste0("(", s, ") ", format(round(v, 4), nsmall = 4)))
    })
  }

  values <- values[names(values) != "intercept"]
  return(values)
}
