#' Error functions
#'
#' Functions measuring the accuracy between original and predicted data. Can
#' be used as target functions for e.g. optimization routines which minimize the
#' error term.
#'
#' The following error functions are available:
#' * Mean Absolute Error (MAE)
#' * Normalized Mean Absolute Error (NMAE)
#' * Root Mean Square Error (RMSE)
#' * Normalized Root Mean Square Error (NRMSE)
#' * Symmetric Mean Absolute Percentage Error (SMAPE)
#' * Sum of Squared Errors (SSE)
#'
#' @rdname errfun
#' @param orig numeric vector of original data points
#' @param pred numeric vector of predicted data points
#' @return numeric error term
#' @export
#' @examples
#' # Mean Absolute Error
#' mae(1:5, 1:5+0.1)
#' # Sum of Squared Errors
#' sse(1:5, 1:5+0.1)
# Mean Absolute Error
mae <- function(orig,pred) {
  sum(abs(orig-pred))/length(orig)
}

# Normalized Mean Absolute Error
#' @rdname errfun
#' @export
nmae <- function(orig,pred) {
  mae(orig,pred)/ifelse(mean(orig)==0,1,mean(orig))
}

# Root Mean Square Error
#' @rdname errfun
#' @export
rmse <- function(orig, pred) {
  sqrt(sum((orig-pred)^2)/length(orig))
}

# Normalized Root Mean Square Error
#' @rdname errfun
#' @export
nrmse <- function(orig, pred) {
  rmse(orig,pred)/ifelse(mean(orig)==0,1,mean(orig))
}

# Symmetric Mean Absolute Percentage Error
#' @rdname errfun
#' @export
smape <- function(orig,pred) {
  100/length(orig)*sum(2*abs(pred-orig)/(abs(orig)+abs(pred)))
}

# Sum of squared errors
#' @rdname errfun
#' @export
sse <- function(orig,pred) {
  sum((orig-pred)^2)
}
