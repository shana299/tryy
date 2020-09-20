#' @exportClass linreg
#' @import stats
#' @export linreg

#' @title Linreg Class Constructor
#' @param x A list
#' @return The same list input with class attribute modified to 'linreg'

# build a 'linreg' class constructor

new_linreg <- function(x = list()) {
  stopifnot(is.list(x))
  structure(x, class = "linreg")
}

#' @title Linear Regression
#' @description The 'linreg' function allows for the linear regression of a dependent variable on
#' a set of independent variables, and thereafter, views of regression coefficients, residuals, and other statistics
#' @note The 'linreg' function was designed to work just as the 'lm' function in the 'stats' 
#' package. The 'linreg' function documentation, therefore, also draws upon the 'lm' function
#' documentation. Review the 'See Also' section
#' @seealso \code{\link[stats]{lm}}
#' @param formula an object of class "formula" (or one that can be coerced to that class):
#' a symbolic description of the model to be fitted. The details of model specification are
#' given under ‘Details’.
#' @param data a data frame containing the variables in the model
#' @param qr_method a boolean; TRUE forces linreg to use QR decomposition for the regression
#' @param ... additional arguments to be passed to the \code{model.matrix} function, see 'Details'
#' @return lm returns an object of class "linreg".
#' The function summary prints a summary table of the results. The generic accessor functions
#' coefficients, effects, fitted.values and residuals extract various useful features of the
#' value returned by linreg.
#' An object of class "linreg" is a list containing at least the following components:
#' \item{call}{the matched call}
#' \item{coefficients}{a named vector of coefficients}
#' \item{fitted_values}{the fitted mean values}
#' \item{residuals}{the residuals, that is response minus fitted values}
#' \item{df}{the residual degrees of freedom}
#' \item{residual_variance}{estimated value of the variance of the residuals}
#' \item{t_values}{t-statistics of the independent variables, that is coefficients/standard-error}
#' \item{p_values}{p-values for the two-sided t-test with null that coefficients are each zero}
#' @examples
#' data("iris")
#' linreg(Petal.Length ~ Species, iris)
#' @references
#' \href{https://en.wikipedia.org/wiki/Linear_regression}{Linear Regression}
#' \href{https://en.wikipedia.org/wiki/QR_decomposition}{QR decomposition of a Matrix}
#' \href{https://genomicsclass.github.io/book/pages/qr_and_regression.html}{Linear Regression with QR decomposition}
#' @details 
#' Models for \code{linreg} are specified symbolically. A typical model has the form \code{response ~ terms} 
#' where response is the (numeric) response vector and terms is a series of terms which specifies a 
#' linear predictor for response. A terms specification of the form \code{first + second} indicates
#' all the terms in first together with all the terms in second with duplicate terms removed. Specifications
#' of the form \code{first:second} or \code{first*second} are recognised exactly the same way as 
#' \code{first+second}. Additional arguments passed via ..., if provided, will force the linear model will have no intercept.

# main function: linreg
linreg <- function(formula, data, qr_method = FALSE, ...) {
  
  # check class of data input
  if (class(data) != "data.frame") stop("Data must be input in the data.frame format")
  
  # parse formula to get all regression (independent) variables along with the response (dependent) variable
  all_vars <- unique(all.vars(formula))
  
  # extract response (independent) variable
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

    coefficients <- as.vector(solve(t(X) %*% X) %*% t(X) %*% y)
    names(coefficients) <- colnames(X)
    preds <- X %*% coefficients
    residuals <- y - preds
    df <- nrow(X) - ncol(X)
    residual_variance <- as.numeric((t(residuals) %*% residuals) / df)
    variance_coeff <- residual_variance * solve(t(X) %*% X)
    
  } else { # with QR
  
    if (invert_issue) {warning("*** Using QR Method for solution ***")}
    df <- nrow(X) - ncol(X)
    QR <- qr(X)
    Q <- qr.Q(QR)
    R <- qr.R(QR)
    
    # coefficients <- qr.coef(X, y)
    coefficients <- as.vector(backsolve(R, crossprod(Q,y)))
    names(coefficients) <- colnames(X)
    # preds <- qr.fitted(X, y)
    preds <- X %*% coefficients
  
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