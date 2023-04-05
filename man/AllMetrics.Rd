\name{all_metrics}
\alias{all_metrics}
\title{Calculating Multiple Performance Metrics of a Prediction Model}
\usage{
all_metrics(actual, predicted)
}
\arguments{
\item{actual}{This is the actual time series values}
\item{predicted}{This is the predicted values of a time series using a model}
}
\value{
\itemize{
  \item AllMetrics - A data frame containing two columns with first column as the name of the eight metrics and second column as the corresponding values
}
}
\description{
This provides a function to calculate multiple performance metrics for actual and predicted values.
}
\examples{
actual <- c(1.5, 2.3, 25, 52, 14)
predicted <- c(1.2, 10, 3.5, 4.3, 5.6)
# Inside the function 1st specify actual then predicted
print(all_metrics(actual, predicted))
}
\references{
\itemize{
\item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
}
}
