#' @exportClass ridgereg
#' @import stats

#' @title Ridgereg Class Constructor
#' @param x A list
#' @return The same list input with class attribute modified to 'ridgereg'

# build a 'ridgereg' class constructor
new_ridgereg <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "ridgereg")
}

#' @title Ridge Regression
#' @description The 'ridgereg' function runs a ridge regression algorithm on a collection of 
#' observations of a dependent variable and corresponding independent variables. Thereafter, it
#' provides views of regression coefficients, residuals, and other statistics
#' @note The 'ridgereg' function was designed to work like the 'lm.ridge' function in the 'MASS' 
#' package. Independent variables are standardised before regression is performed; the predict method
#' on the returned object will use the same means and standard deviations used for standardisation of
#' the training dataset. Review the 'See Also' section.
#' @seealso \code{\link[MASS]{lm.ridge}}
#' @seealso \code{\link[stats]{lm}}
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted. The details of model specification are
#' given under ‘Details’.
#' @param data a data frame containing the variables in the model
#' @param lambda hyperparameter for ridge regression
#' @param qr_method a boolean; TRUE forces ridgereg to use QR decomposition for the regression
#' @param ... additional arguments to be passed to the \code{model.matrix} function, see 'Details'
#' @return the function returns an object of class "ridgereg", which has a number of associated 
#' methods including \code{\link[linear.regression]{print.ridgereg}}, 
#' \code{\link[linear.regression]{predict.ridgereg}},
#' and \code{\link[linear.regression]{coef.ridgereg}}.
#' The "ridgereg" object itself is a list containing at least the following components:
#' \item{call}{the matched call}
#' \item{coefficients}{a named vector of coefficients}
#' \item{fitted_values}{the fitted mean values}
#' \item{residuals}{the residuals, that is response minus fitted values}
#' \item{df}{the residual degrees of freedom}
#' \item{residual_variance}{estimated value of the variance of the residuals}
#' \item{t_values}{t-statistics of the independent variables, that is coefficients/standard-error}
#' \item{p_values}{p-values for the two-sided t-test with null that coefficients are each zero}
#' \item{std_means}{a vector of means of the independent variables in the training dataset}
#' \item{std_sd}{a vector of standard deviations of the independent variables in the training dataset}
#' @examples
#' library(mlbench)
#' data(BostonHousing)
#' ridgereg(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad, BostonHousing, lambda = 1)
#' @details 
#' Models for \code{linreg} are specified symbolically. A typical model has the form \code{response ~ terms} or
#' \code{response ~ .}, where 'response' is the (numeric) response vector. In the former form, 'terms' is a series
#' of terms which specifies a linear predictor for response. In the latter form, the '.' indicates that all available
#' predictors must be taken into account for modelling. Term specification of the form \code{first + second}
#' indicates all the terms in first together with all the terms in second with duplicate terms removed.
#' Specifications of the form \code{first:second} or \code{first*second} are recognised exactly the same way as 
#' \code{first+second}. Additional arguments passed via ..., if provided, will force the linear model will 
#' have no intercept.
#' @export

# main function: ridgereg
ridgereg <- function(formula, data, lambda, qr_method = FALSE, ...) {
  
  # check class of data input
  if (class(data) != "data.frame") stop("Data must be input in the data.frame format")
  
  # parse formula to get all regression (independent) variables along with the response (dependent) variable
  all_vars <- unique(all.vars(formula))
  if (all_vars[2] == ".") {all_vars <- c(all_vars[1], colnames(data)[colnames(data) != all_vars[1]])}
  
  # extract response (independent) variable
  y <- data[, all_vars[1]]

  # check that dependent variable is numeric, else print an error message
  if (!is.numeric(y)) stop("Dependent variable must be numeric")
  
  # check if independent variables (viz. all_vars[-1]) are neither numeric nor factor, if so print an error message
  indep_var_classes <- sapply(all_vars[-1], function(x) {class(data[, x])})
  indep_var_classes_unique <- unique(indep_var_classes)
  
  if(!all(indep_var_classes %in% c("factor", "numeric", "single", "double"))) {
    stop("Independent variables can only either be numeric or factor types")
  }
  
  # extract independent variables, viz. all_vars[-1]
  X <- data[, all_vars[-1], drop=FALSE]
  
  # get design matrix
  add_intercept <- if (length(list(...)) > 0) {FALSE} else {TRUE}
  factor_indep_vars <- names(indep_var_classes[indep_var_classes == "factor"])
  X <- model.matrix(X, factor_indep_vars, add_intercept)
  
  # standardise variables (leave intercept out)
  num_params <- ncol(X) # includes intercept
  std_means <- sapply(2:num_params, function(x) {mean(X[, x])})
  std_sd <- sapply(2:num_params, function(x) {sd(X[, x])})
  X[, 2:num_params] <- sapply(2:num_params, function(x) {
    (X[, x] - std_means[x - 1]) / std_sd[x - 1]})
  
  # check for design matrix invertibility issues
  lhs <- crossprod(X) + diag(lambda, num_params, num_params)
  inverted <- try(solve(lhs, silent = FALSE))
  invert_issue <- suppressWarnings(!(class(inverted)[1] == "matrix"))
  
  if (qr_method == FALSE & !invert_issue) { # without QR
    
    coefficients <- as.vector(inverted %*% crossprod(X, y))
    names(coefficients) <- colnames(X)
    
  } else { # with QR
    
    if (invert_issue) {warning("*** Using QR Method for solution ***")}
    df <- nrow(X) - num_params
    QR <- qr(lhs)
    Q <- qr.Q(QR)
    R <- qr.R(QR)
    
    # coefficients <- qr.coef(X, y)
    coefficients <- as.vector(backsolve(R, crossprod(Q, crossprod(X, y))))
    names(coefficients) <- colnames(X)
    
  }
  
  preds <- X %*% coefficients
  residuals <- y - preds
  df <- nrow(X) - num_params
  residual_variance <- as.numeric(crossprod(residuals) / df)
  variance_coeff <- residual_variance * solve(crossprod(X))
  t_values <- coefficients / sqrt(diag(variance_coeff))
  p_values <- sapply(2*(1 - stats::pt(abs(t_values), df)), function(x) {
    if(x < 2e-16) {"<2e-16"} else x})
  
  # assemble and return output object
  return_object <- list(call = match.call(), coefficients = coefficients, fitted_values = preds,
                        residuals = residuals, df = df, residual_variance = residual_variance,
                        variance_of_coefficients = variance_coeff, t_values = t_values, 
                        p_values = p_values, std_means = std_means, std_sd = std_sd, 
                        all_vars = all_vars)
  
  return(new_ridgereg(return_object))
  
}