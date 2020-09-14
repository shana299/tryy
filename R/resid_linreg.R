#' @export resid

# generic
resid <- function(x) {UseMethod("resid")}

#' @export

# s3 method
resid.linreg <- function(x, ...) {x$residuals}