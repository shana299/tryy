#' @export pred

# generic
pred <- function(x) {UseMethod("pred")}


#' @export

# s3 method
pred.linreg <- function(x, ...) {x$fitted_values}