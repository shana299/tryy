#' @title S3 Generic function
#' @param x an object
#' @export

# generic
resid <- function(x) {UseMethod("resid")}

#' @title Return Linear Regression Residuals
#' @description An S3 function to return residuals from a \code{linreg} object
#' @param x an object of class \code{linreg}
#' @return a vector containing the residuals
#' @export

# s3 method
resid.linreg <- function(x) {x$residuals}