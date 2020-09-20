#' @title Theme function to be used with ggplots 
#' @import ggplot2
#' @export theme_liu

theme_liu <- function(){
  #
  theme_minimal() %+replace%    
    
    theme(
      
      panel.grid.major =  element_line(colour = '#f4e748', size = 1, linetype = "dashed", lineend = "square"),   
      panel.grid.minor = element_line(colour = '#877ad1', size = 0.6, linetype = "dotted", lineend = "square"),    
      axis.ticks = element_blank(),          
      
      
      panel.background = element_rect(fill = '#FFFFFF', colour = '#00b1d9', size = 2)
    )
}