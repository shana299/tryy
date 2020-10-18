#' @import nycflights13
#' @importFrom dplyr inner_join select group_by summarise mutate
#' @importFrom utils data

#' @title Plots Mean Arrival Delays of US Domestic Flights from NYC
#' @return A Plot of the Mean Arrival Delays of US Domestic Flights from NYC
#' @export

visualize_airport_delays <- function() {

  arr_delay <- lat <- lon <- mean_delay <- mean_delay_grp <- NULL # provide binding and prevent devtools::check() from returning a related note
  
  data <- dplyr::inner_join(nycflights13::airports, nycflights13::flights, by = c("faa" = "dest")) %>%
    dplyr::select(lat, lon, arr_delay) %>% dplyr::group_by(lat, lon) %>% 
    dplyr::summarise(mean_delay = mean(arr_delay, na.rm = TRUE)) %>%
    dplyr::mutate(mean_delay_grp = cut(mean_delay, breaks = c(-40, -20, 0, 20, 40, 60), include.lowest = TRUE))
  
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = lon, y = lat, size = mean_delay, color = mean_delay_grp)) +
    ggplot2::geom_point(alpha = 0.5) + ggplot2::scale_color_discrete(name = "Mean Delay Bucket (mins)") + 
    ggplot2::scale_size(name = "Mean Delay (mins)") + ggplot2::ylab("Latitude") + ggplot2::xlab("Longitude")
  
}