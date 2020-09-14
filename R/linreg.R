#' @exportClass linreg
#' @export new_linreg
#' @import stats
#' @export linreg

# build a 'linreg' class constructor

new_linreg <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "linreg")
}

# main function: linreg
linreg <- function(formula, data, qr_method = FALSE, ...) {
  
  # check class of data input
  if (class(data) != "data.frame") stop("Data must be input in the data.frame format")
  
  # parse formula to get all regression (independent) variables along with the response (dependent) variable
  all_vars <- all.vars(formula)
  
  # process data
  y <- data[, all_vars[1]]
  
  # check that dependent variable is numeric, else print an error message
  if (!is.numeric(y)) stop("Dependent variable must be numeric")
  
  # check if independent variables (viz. all_vars[-1]) are neither numeric nor factor, if so print an error message
  indep_var_classes <- sapply(all_vars[-1], function(x) {class(data[, x])})
  indep_var_classes_unique <- unique(indep_var_classes)
  
  if(!all(indep_var_classes %in% c("factor", "numeric"))) stop("Independent variables can only either be numeric or factor types")
  
  # extract independent variables, viz. all_vars[-1]
  X <- data[, all_vars[-1], drop=FALSE]
  
  # get design matrix
  add_intercept <- if (length(list(...)) > 0) {FALSE} else {TRUE}
  factor_indep_vars <- names(indep_var_classes[indep_var_classes == "factor"])
  X <- model.matrix(X, factor_indep_vars, add_intercept)

  # check for design matrix invertibility issues
  invert_issue <- suppressWarnings(!(class(try(solve(t(X) %*% X), silent = FALSE))[1] == "matrix"))
  
  # calculate output

  if (qr_method == FALSE & !invert_issue) { # without QR

    coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
    names(coefficients) <- colnames(X)
    preds <- X %*% coefficients
    residuals <- y - preds
    df <- nrow(X) - ncol(X)
    residual_variance <- as.numeric((t(residuals) %*% residuals) / df)
    variance_coeff <- residual_variance * solve(t(X) %*% X)
    
  } else { # with QR
  
    if (invert_issue) {print("*** Using QR Method for solution ***")}
    df <- nrow(X) - ncol(X)
    X <- qr(X)
    R <- qr.R(X)
    
    coefficients <- qr.coef(X, y)
    preds <- qr.fitted(X, y)
    residuals <- y - preds
    residual_variance <- as.numeric((t(residuals) %*% residuals) / df)
    variance_coeff <- residual_variance * solve(t(R) %*% R)
    
  }
  
  t_values <- coefficients / sqrt(diag(variance_coeff))
  p_values <- sapply(2*(1 - stats::pt(abs(t_values), df)), function(x) {if(x < 2e-16) {"<2e-16"} else x})
  
  # assemble and return output object
  return_object <- list(call = match.call(), coefficients = coefficients, fitted_values = preds,
                        residuals = residuals, df = df, residual_variance = residual_variance,
                        variance_of_coefficients = variance_coeff, t_values = t_values, p_values = p_values)
  
  return(new_linreg(return_object))
}