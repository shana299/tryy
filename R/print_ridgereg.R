#' @title Print Out \code{ridgereg} Function Call and Coefficients
#' @param x an object of class \code{ridgereg}
#' @param ... additional parameters
#' @export

print.ridgereg <- function(x, ...) {
  y <- round(as.numeric(x$coefficients), 3)
  names(y) <- names(x$coefficients)
  cat("Call:\n")
  print(x$call)
  cat("\n\nCoefficients:\n")
  print(y)
}