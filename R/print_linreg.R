#' @title Print Function Call and Regression Coefficients
#' @param x an object of class \code{linreg}
#' @param ... additional parameters
#' @export

print.linreg <- function(x, ...) {
  y <- round(as.numeric(x$coefficients), 3)
  names(y) <- names(x$coefficients)
  cat("Call:\n")
  print(x$call)
  cat("\n\nCoefficients:\n")
  print(y)
}