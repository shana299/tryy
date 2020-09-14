#' @export model.matrix

# generic
model.matrix <- function(X, ...) UseMethod("model.matrix")

#' @export

# s3 method
model.matrix.data.frame <- function(X, factor_indep_vars, add_intercept, ...) {
  
  # run one-hot encoding for 'factor' type independent variables
  print(factor_indep_vars)
  for (var in factor_indep_vars) {
    
    factor_levels <- levels(X[, var])
    
    if (length(factor_levels) == 1) {
      X[, var] <- 1 # degenerate case, convert factor into numeric
    } else {
      for (lev in factor_levels[-1]) {
        X[, paste0(var,lev)] <- sapply(X[, var], function(x) {if(x == lev) {1} else {0}}) # one-hot encoding
      }
      X <- X[, colnames(X) != var] # remove original factor variable
    }
  }
  
  # add intercept if necessary
  
  if (add_intercept) {
    X <- cbind(rep(1, nrow(X)), X)
    colnames(X)[1] <- "(Intercept)"
  }
  
  return(as.matrix(X))
}