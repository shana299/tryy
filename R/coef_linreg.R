#' @title Get Linear Regression Coefficients
#' @description An S3 function to return coefficients from a \code{linreg} object
#' @param object an object of class \code{linreg}
#' @param ... additional arguments
#' @return a vector containing the regression coefficients
#' @export

# s3 method

coef.linreg <- function(object, ...) {object$coefficients}