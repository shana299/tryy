# create generic functions needed
linreg <- function(x, ...) UseMethod("linreg")

# create specific methods

linreg.formula <- function(formula, data, add_intercept = TRUE) {
  
  # process data
  processed_data <- model.matrix(data, formula, add_intercept)
  y <- processed_data$y # dependent (response) variable
  X <- processed_data$X # design matrix

  # assemble output
  call <- paste0("linreg(formula = ", deparse(formula), ", data = ", deparse(substitute(data)), ")")
  coefficients <- solve(t(X) %*% X) %*% t(X) %*% y
  names(coefficients) <- colnames(X)
  preds <- X %*% coefficients
  residuals <- y - preds
  df <- nrow(X) - ncol(X)
  residual_variance <- as.numeric((t(residuals) %*% residuals) / df)
  variance_coeff <- residual_variance * solve(t(X) %*% X)
  t_values <- coefficients / sqrt(diag(variance_coeff))
  p_values <- sapply(2*(1 - pt(t_values/2, df)), function(x) {if(x < 2e-16) {"<2e-16"} else x})
  
  return_object <- list(call = call, coefficients = coefficients, fitted_values = preds,
                        residuals = residuals, df = df, residual_variance = residual_variance,
                        variance_of_coefficients = variance_coeff, t_values = t_values, p_values = p_values)
  class(return_object) <- "linreg"
  
  # return output
  return(return_object)
}

print.linreg <- function(x, ...) {
  cat("Call:\n", x$call,"\n\nCoefficients:\n")
  named_coeff_vector <- as.vector(x$coefficients)
  names(named_coeff_vector) <- names(x$coefficients)
  print(named_coeff_vector)
}

plot.linreg <- function(x, ...) {
  
  # import ggplot2 library using the call usethis::use_package("ggplot2")
  # this will prompt roxygen2 to add ggplot2 to the Imports field in DESCRIPTION
  
  
}

resid.linreg <- function(x, ...) {x$residuals}

pred.linreg <- function(x, ...) {x$fitted_values}

coef.linreg <- function(x, ...) {
  named_coeff_vector <- as.vector(x$coefficients)
  names(named_coeff_vector) <- names(x$coefficients)
  print(named_coeff_vector)
}

summary.linreg <- function(x, ...) {
  summ_df <- as.data.frame(x$coefficients)
  summ_df[, 2] <- sqrt(diag(x$variance_of_coefficients))
  summ_df[, 3] <- x$t_values
  summ_df[, 4] <- x$p_values
  colnames(summ_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  cat("Coefficients:\n")
  print(summ_df)
  cat("---\n")
  cat("Residual standard error:", sqrt(x$residual_variance), "on", x$df, "degrees of freedom")
}
