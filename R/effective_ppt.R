#' Effective precipitation
#'
#' @param input_data A dataframe that contains columns 'Date', 'Tmin', 'Tmax', 'PPT'
#' @param threshold A minimum amount of precipitation required before water reaches the peatland storage.
#' @param column Precipitation variable in centimeters
#' @return A dataframe that includes both the original columns and the newly calculated effective precipitation values.
#' @export
#' @examples
#' input_data <- data.frame(Date = as.Date(c("2024-01-01", "2024-01-02",
#' "2023-01-03")), Tmin = c(-36.1, -23.8, -25.6), Tmax = c(-27.0, -7.5, -12.4),
#' PPT = c(0.0, 2.1, 0.1))
#' effective_ppt(input_data, column = c("PPT"), threshold = 0.01)
effective_ppt <- function(input_data, column, threshold){

  # Check for "Date" Column
  if (!"Date" %in% colnames(input_data)) {
    stop("Error: The dataset must contain a 'Date' column formatted as 'YYYY-MM-DD'.")
  }

  input_data$eff_Precip_cm <- NA

  input_data <- dplyr::mutate(input_data, eff_Precip_cm = dplyr::case_when(
    !!sym(column)  < threshold ~ 0,  # Set negative or small values to 0
      TRUE ~ !!sym(column)       # Keep other values unchanged
    ))


  # Check for "Date" Column
  if (!"eff_Precip_cm" %in% colnames(input_data)) {
    stop("Error: The dataset must contain a 'Date' column formatted as 'YYYY-MM-DD'.")
  }

  # Plot the effective precipitation
  plot1 <- ggplot2::ggplot(input_data, aes(x = Date, y = eff_Precip_cm)) +
    geom_col(color = "blue") +
    scale_x_date(
      name = "Date",
      date_breaks = "1 year",
      labels = scales::date_format("%Y"),  # Explicitly call `scales::date_format()`
      expand = c(0, 0)
    ) +
    scale_y_continuous(quote("Effective Precipitation (cm)"),
                       expand = c(0.004, 0.004)) +
    theme(
      panel.background = element_rect(fill = "white", colour = "black"),
      axis.text.x = element_text(angle = 0, size = 18, hjust = -2, color = "black"),
      axis.text.y = element_text(size = 18, colour = "black"),
      axis.title.x = element_blank(),
      legend.direction = "horizontal",
      legend.text = element_text(size = 14),
      legend.title = element_blank(),
      legend.background = element_blank(),
      legend.key.size = unit(0.8, "cm"),
      legend.key.width = unit(1, "cm"),
      legend.key = element_rect(colour = NA, fill = NA),
      axis.title.y = element_text(size = 18, colour = "black"),
      plot.title = element_text(hjust = 0.01, vjust = -8, size = 18),
      plot.margin = margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  # Print the plot to display automatically
  print(plot1)

  message("Saving Effective Precipitation plot...")

  try(
    ggplot2::ggsave("output/Effective_Precipitation.png", plot=plot1, width=14, height=6, dpi=300), silent=FALSE
  )

  message("Plot saved!")

  return(input_data)
}
