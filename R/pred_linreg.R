#' @title S3 generic
#' @param x an object
#' @export

# generic
pred <- function(x) {UseMethod("pred")}

#' @title Return Modelled Response Values (predictions given training data)
#' @description An S3 function to return modelled response values from a \code{linreg} object
#' @param x an object of class \code{linreg}
#' @return a vector containing the modelled response values
#' @export

# s3 method
pred.linreg <- function(x) {x$fitted_values}