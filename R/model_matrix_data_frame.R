#' @export

# define model.matrix S3 method for data.frame objects

model.matrix.data.frame <- function(data, formula, add_intercept, ...) {
  
  # parse formula to get all regression (independent) variables along with the response (dependent) variable
  all_vars <- all.vars(formula)
  
  # extract dependent variable, viz. all_vars[1]
  y <- data[, all_vars[1]]
  
  # check that dependent variable is numeric, else print an error message
  if (!is.numeric(y)) stop("Dependent variable must be numeric")
  
  # check if independent variables (viz. all_vars[-1]) are neither numeric nor factor, if so print an error message
  indep_var_classes <- sapply(all_vars[-1], function(x) {class(data[, x])})
  indep_var_classes_unique <- unique(indep_var_classes)
  
  if(!all(indep_var_classes %in% c("factor", "numeric"))) stop("Independent variables can only either be numeric or factor types")
  
  # extract indepedent variables, viz. all_vars[-1]
  X <- data[, colnames(data) %in% all_vars[-1], drop=FALSE]
  
  # run one-hot encoding for 'factor' type independent variables
  factor_indep_vars <- names(indep_var_classes[indep_var_classes == "factor"])
  
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
  
  return(list(X = as.matrix(X), y = y))
}