#' @import ggplot2
#' @export

plot.linreg <- function(x, num_labels = 3, labels_by_val = FALSE, ...) {
  
  require(ggplot2) # needing to use this despite adding ggplot2 to DESCRIPTION file under Imports via usethis::use_package("ggplot2")
  
  df_for_plot <- data.frame(fitted_values = x$fitted_values, residuals = x$residuals)
  top_n <- order(abs(x$residuals), decreasing = TRUE)[1:num_labels]
  
  # plot Residuals Vs Fitted
  
  if (!labels_by_val) {
    
    p1 <- ggplot(df_for_plot, aes(x=fitted_values, y=residuals)) +
      geom_smooth(method="loess", aes(colour="red"), se = FALSE, show.legend = FALSE, formula = y~x) +
      geom_point(shape = 1) + labs(title="Residuals Vs Fitted", y="Residuals", x="Fitted values") +
      theme(plot.title = element_text(hjust = 0.5)) + theme_classic() + 
      geom_hline(yintercept = 0, linetype="dotted") + 
      geom_text(data=df_for_plot[top_n, ], aes(label=rownames(df_for_plot[top_n, ])), nudge_x = 0.2)
    
  } else {
    
    p1 <- ggplot(df_for_plot, aes(x=fitted_values, y=residuals, label=residuals)) +
      geom_smooth(method="loess", aes(colour="red"), se = FALSE, show.legend = FALSE, formula = y~x) +
      geom_point(shape = 1) + labs(title="Residuals Vs Fitted", y="Residuals", x="Fitted values") +
      theme(plot.title = element_text(hjust = 0.5)) + theme_classic() +
      geom_hline(yintercept = 0, linetype="dotted") + 
      geom_text(data=df_for_plot[top_n, ], nudge_x = 0.2)
    
  }
  
  # plot Scale-Location
  
  df_for_plot$std_res <- (df_for_plot$residual - mean(df_for_plot$residual))/sd(df_for_plot$residual)
  
  if (!labels_by_val) {
    
    p2 <- ggplot(df_for_plot, aes(x=fitted_values, y=sqrt(abs(std_res)))) + geom_point(shape = 1) +
      geom_smooth(method="loess", aes(colour="red"), se = FALSE, show.legend = FALSE, formula = y~x) + 
      labs(title="Scale-Location", y=expression(sqrt(abs(paste("Standardised ","Residuals")))), x="Fitted values") +
      theme(plot.title = element_text(hjust = 0.5)) + theme_classic() + 
      geom_text(data=df_for_plot[top_n, ], aes(label=rownames(df_for_plot[top_n, ])), nudge_x = 0.2)
    
  } else {
    
    p2 <- ggplot(df_for_plot, aes(x=fitted_values, y=sqrt(abs(std_res)))) + geom_point(shape = 1) +
      geom_smooth(method="loess", aes(colour="red"), se = FALSE, show.legend = FALSE, formula = y~x) + 
      labs(title="Scale-Location", y=expression(sqrt(abs(paste("Standardised ","Residuals")))), x="Fitted values") +
      theme(plot.title = element_text(hjust = 0.5)) + theme_classic() + 
      geom_text(data=df_for_plot[top_n, ], nudge_x = 0.2)
    
  }
  
  suppressWarnings(plot(p1))
  suppressWarnings(plot(p2))
  
}