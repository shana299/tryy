#' @export

coef.linreg <- function(x, ...) {
  named_coeff_vector <- as.vector(x$coefficients)
  names(named_coeff_vector) <- names(x$coefficients)
  print(named_coeff_vector)
}