#' @title Summarise Linear Regression output
#' @param object an object of class \code{linreg}
#' @param ... additional parameters
#' @export

# s3 method

summary.linreg <- function(object, ...) {
  summ_df <- as.data.frame(object$coefficients)
  summ_df[, 2] <- round(sqrt(diag(object$variance_of_coefficients)), 5)
  summ_df[, 3] <- round(object$t_values, 5)
  summ_df[, 4] <- sapply(object$p_values, 
                         function(x) if(is.numeric(x)) {round(x, 5)} else {x})
  summ_df[, 5] <- sapply(object$p_values, 
                         function(x) if(x < 0.001) {"***"} 
                         else if (x < 0.01) {"**"} 
                         else if (x < 0.05) {"*"} 
                         else if (x < 0.1) {"."} 
                         else {" "})
  
  colnames(summ_df) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "")
  cat("Coefficients:\n")
  print(summ_df)
  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat("Residual standard error:", sqrt(object$residual_variance), "on", 
      object$df, "degrees of freedom")
}