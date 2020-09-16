#' @title Print Function Call and Regression Coefficients
#' @param x an object of class \code{linreg}
#' @param ... additional parameters
#' @export

print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n\nCoefficients:\n")
  print(x$coefficients)
}