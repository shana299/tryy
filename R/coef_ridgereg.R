#' @title Get Ridge Regression Coefficients
#' @description An S3 function to return coefficients from a \code{ridgereg} object
#' @param object an object of class \code{ridgereg}
#' @param ... additional arguments
#' @return a vector containing the regression coefficients
#' @export

# s3 method

coef.ridgereg <- function(object, ...) {object$coefficients}