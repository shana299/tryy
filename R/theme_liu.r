#' @title Theme function to be used with ggplots 
#' @import ggplot2
#' @export theme_liu

theme_liu <- function(){
  
  ggplot2::theme_minimal() %+replace%    
    
    ggplot2::theme(
      
      panel.grid.major = ggplot2::element_blank(),    
      panel.grid.minor = ggplot2::element_blank(),    
      axis.ticks = ggplot2::element_blank(),          
      
      
      panel.background = ggplot2::element_rect(fill = "red", colour = "green")
    )
}