#' @title S3 Generic function - rewritten
#' @param X an object
#' @param factor_indep_vars an object
#' @param add_intercept an object
#' @seealso \link{model.matrix.data.frame}
#' @export

model.matrix <- function(X, factor_indep_vars, add_intercept) UseMethod("model.matrix")

#' @title Get Design Matrix
#' @description An S3 function to return a design matrix given a \code{data.frame} and
#' a linear regression \code{formula}
#' @param X a \code{data.frame}
#' @param factor_indep_vars a vector of names of independent variables whose classes are of \code{factor} type
#' @param add_intercept a boolean; TRUE requires the design matrix to include a column for an intercept
#' @return the design matrix
#' @export

# s3 method
model.matrix.data.frame <- function(X, factor_indep_vars, add_intercept) {
  
  # run one-hot encoding for 'factor' type independent variables
  
  for (var in factor_indep_vars) {
    
    factor_levels <- levels(X[, var])
    
    if (length(factor_levels) == 1) {
      X[, var] <- 1 # degenerate case, convert factor into numeric
    } else if (all(factor_levels %in% c(0, 1))) { # 0 and 1 are the only factors
      
      X[, var] <- as.numeric(X[, var]) # simply convert factor into numeric
      
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