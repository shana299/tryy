#' @export

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