#' @exportClass linreg
#' @export new_linreg
#' @export linreg

# build a 'linreg' class constructor

new_linreg <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "linreg")
}

# create main function to build the linear regression model

linreg <- function(formula, data, add_intercept = TRUE, qr_method = FALSE) {
  
  # process data
  processed_data <- model.matrix(data, formula, add_intercept)
  y <- processed_data$y # dependent (response) variable
  X <- processed_data$X # design matrix
  
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
  p_values <- sapply(2*(1 - pt(abs(t_values), df)), function(x) {if(x < 2e-16) {"<2e-16"} else x})
  
  # assemble and return output object
  return_object <- list(call = match.call(), coefficients = coefficients, fitted_values = preds,
                        residuals = residuals, df = df, residual_variance = residual_variance,
                        variance_of_coefficients = variance_coeff, t_values = t_values, p_values = p_values)
  
  return(new_linreg(return_object))
}