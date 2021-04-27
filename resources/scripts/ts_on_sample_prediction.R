#'
#' @author Lionel Lamy
#' @concept OneAhead redesigned
#'
#' @description
#' Calculates the mean squared error of n.lag steps
#' predicted for a time series modeled according to a (S)AR(I)MA.
#'
#' @details
#' For the last 20% data of the ts, compute a model with [1:i] data points
#' and predict n.lag steps ahead, up to the length of the ts.
#'
#' @note
#' Be careful not to set the orders value too high.
#' The data must be the original one, not differentiated.
#' The predicted values returned are the first step predicted, independent of n.lag
#'
#' @param data the (univariate) time series. Must be class of ts().
#' @param order the order of the time series, for the model.
#' @param seasonal the seasonal order of the time series, for the model.
#' @param n.lag the n.ahead steps to be predicted with predict().
#'
#' @return
#' Returns a list with
#' - pred : the 20% predicted points
#' - se : the values of each squared errors
#' - mse : the mean of se, aka mean squared errors : sum(se)/n
#'
#' @examples
#' on.sample.prediction(gas, order = c(1, 1, 1), seasonal = list(order = c(1, 1, 1), period = 12))

on.sample.prediction <- function(data, order, seasonal = list(order = c(0, 0, 0), period = NA), n.lag = 1) {
  
  if (!is(data, "ts")) {
    warning("The argument 'data' must be a time series object!", immediate. = T)
    return(NA)
  }
  
  ts.n <- length(data)
  ts.80 <- floor(.8 * ts.n)
  ts.seq <- seq(ts.80, ts.n) - 1

  # For each iteration, compute the n.lag future value of a model based on ts[1:i]
  predictions <- sapply(ts.seq, function(i) {
    cnt <- ts.n - i
    model <- arima(data[1:i], order = order, seasonal = seasonal)
    head(predict(model, n.ahead = n.lag)$pred, cnt)
  })

  # Compute the square of errors
  errors <- sapply(1:length(predictions), function(i) {
    pred <- predictions[[i]]
    index.start <- ts.seq[i]
    index.end <- index.start + length(pred)
    true.val <- data[I(index.start + 1):index.end]

    error <- (true.val - pred)^2
    
    # could also implements other criterions like deviance
    return(error)
  })
  
  errors <- unlist(errors)

  # Retrieve the 1st prediction of each predict()
  pred <- sapply(predictions, "[[", 1)

  return(list(pred = pred, se = errors, mse = mean(errors)))
}
