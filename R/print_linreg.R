#' @export

print.linreg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n\nCoefficients:\n")
  named_coeff_vector <- as.vector(x$coefficients)
  names(named_coeff_vector) <- names(x$coefficients)
  print(named_coeff_vector)
}