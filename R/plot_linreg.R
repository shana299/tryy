#' @import ggplot2
#' @import stats

#' @title Plots of Linear Regression Residuals
#' @description An S3 function to return plots from a \code{linreg} object
#' @param x an object of class \code{linreg}
#' @param num_labels an integer, specifying the number of extreme residuals to label
#' @param labels_by_val a boolean; TRUE forces the function to label selected extreme points by value
#' @param ... additional arguments
#' @return a vector containing the regression coefficients
#' @export

plot.linreg <- function(x, num_labels = 3, labels_by_val = FALSE, ...) {
  
  fitted_values <- std_res <- NULL
  
  # add ggplot2 to DESCRIPTION file under Imports via usethis::use_package("ggplot2")
  
  df_for_plot <- data.frame(fitted_values = x$fitted_values, residuals = x$residuals)
  top_n <- order(abs(x$residuals), decreasing = TRUE)[1:num_labels]
  
  # plot Residuals Vs Fitted
  
  if (!labels_by_val) {
    
    p1 <- ggplot2::ggplot(df_for_plot, ggplot2::aes(x=fitted_values, y=residuals)) +
      ggplot2::geom_smooth(method="loess", ggplot2::aes(colour="red"), se = FALSE, 
                           show.legend = FALSE, formula = y~x) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::labs(title="Residuals Vs Fitted", y="Residuals", x="Fitted values") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + theme_liu() + 
      ggplot2::geom_hline(yintercept = 0, linetype="dotted") + 
      ggplot2::geom_text(data=df_for_plot[top_n, ], ggplot2::aes(label=rownames(df_for_plot[top_n, ])), nudge_x = 0.2)
    
  } else {
    
    p1 <- ggplot2::ggplot(df_for_plot, ggplot2::aes(x=fitted_values, y=residuals, label=residuals)) +
      ggplot2::geom_smooth(method="loess", ggplot2::aes(colour="red"), se = FALSE, 
                           show.legend = FALSE, formula = y~x) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::labs(title="Residuals Vs Fitted", y="Residuals", x="Fitted values") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + theme_liu() +
      ggplot2::geom_hline(yintercept = 0, linetype="dotted") + 
      ggplot2::geom_text(data=df_for_plot[top_n, ], nudge_x = 0.2)
    
  }
  
  # plot Scale-Location
  
  df_for_plot$std_res <- (df_for_plot$residual - mean(df_for_plot$residual))/stats::sd(df_for_plot$residual)
  
  if (!labels_by_val) {
    
    p2 <- ggplot2::ggplot(df_for_plot, ggplot2::aes(x=fitted_values, y=sqrt(abs(std_res)))) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_smooth(method="loess", ggplot2::aes(colour="red"), se = FALSE, 
                           show.legend = FALSE, formula = y~x) + 
      ggplot2::labs(title="Scale-Location", y=expression(sqrt(abs(paste("Standardised ","Residuals")))), 
                    x="Fitted values") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + theme_liu() + 
      ggplot2::geom_text(data=df_for_plot[top_n, ], ggplot2::aes(label=rownames(df_for_plot[top_n, ])), 
                         nudge_x = 0.2)
    
  } else {
    
    p2 <- ggplot2::ggplot(df_for_plot, ggplot2::aes(x=fitted_values, y=sqrt(abs(std_res)))) + 
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_smooth(method="loess", ggplot2::aes(colour="red"), se = FALSE, 
                           show.legend = FALSE, formula = y~x) + 
      ggplot2::labs(title="Scale-Location", y=expression(sqrt(abs(paste("Standardised ","Residuals")))), 
                    x="Fitted values") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) + theme_liu() + 
      ggplot2::geom_text(data=df_for_plot[top_n, ], nudge_x = 0.2)
    
  }
  
  suppressWarnings(plot(p1))
  suppressWarnings(plot(p2))
  
}