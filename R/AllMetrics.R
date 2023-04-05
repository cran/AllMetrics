#'@title Calculating Multiple Performance Metrics of a Prediction Model
#' @description This provides a function to calculate multiple performance metrics for actual and predicted values.
#' @param actual This is the actual time series values
#' @param predicted This is the predicted values of a time series using a model
#'
#' @return
#' \itemize{
#'   \item AllMetrics - A data frame containing two columns with first column as the name of the eight metrics and second column as the corresponding values
#' }
#' @export
#'
#' @examples
#' actual <- c(1.5, 2.3, 25, 52, 14)
#' predicted <- c(1.2, 10, 3.5, 4.3, 5.6)
#' # Inside the function 1st specify actual then predicted
#' print(all_metrics(actual, predicted))
#' @references
#' \itemize{
#'\item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#' }
# all metrics
all_metrics <- function(actual, predicted) {
  # Calculate the residuals
  residuals <- actual - predicted
  abs_residuals <- abs(actual - predicted)
  scaled_abs_residuals <- abs_residuals/actual
  lag_frame <- data.frame(embed(actual,2))
  diff <- lag_frame[,1]-lag_frame[,2]
  abs_diff <- abs(diff)
  # Calculate simple metrics
  mse <- mean(residuals^2)
  rmse <- sqrt(mse)
  rrmse <- 100*rmse/mean(actual)
  mae <- mean(abs_residuals)
  mape <- 100*mean(scaled_abs_residuals)
  mase <- mae/mean(abs_diff)
  # calculate complex matrics
  nse <- 1- (mse/(mean(actual^2)-(mean(actual))^2))
  wi <- 1- (mse/mean((abs(actual-mean(actual))+abs(predicted-mean(actual)))^2))
  lme <- 1- mae/mean(abs(actual-mean(actual)))
  # creating the data frame
  AllMetrics <- data.frame(cbind(c('RMSE', 'RRMSE',
                                    'MAE', 'MAPE',
                                    'MASE','NSE',
                                    'WI', 'LME'),
                                  c(round(rmse,3), round(rrmse,3),
                                    round(mae,3), round(mape,3),
                                    round(mase,3), round(nse,3),
                                    round(wi,3), round(lme,3))))
  colnames(AllMetrics) <- c('Metrics','Values')
  dimnames(AllMetrics)
  dim(AllMetrics)
  # returning the table containing all the metrics
  return(AllMetrics)
}
