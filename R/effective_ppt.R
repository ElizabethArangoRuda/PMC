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
#' effective_ppt(input_data, column = "PPT", threshold = 0.1)

effective_ppt <- function(input_data, column, threshold){

  # Check for "Date" Column
  if (!"Date" %in% colnames(input_data)) {
    stop("Error: The dataset must contain a 'Date' column formatted as 'YYYY-MM-DD'.")
  }

  input_data$eff_Precip_cm <- NA


  input_data <- dplyr::mutate(input_data, eff_Precip_cm = dplyr::case_when(
    input_data[[column]]   < threshold ~ 0,  # Set negative or small values to 0
      TRUE ~ input_data[[column]]          # Keep other values unchanged
    ))

  # Check for "Date" Column
  if (!"eff_Precip_cm" %in% colnames(input_data)) {
    stop("Error: The dataset must contain a 'Date' column formatted as 'YYYY-MM-DD'.")
  }

  start_year <- as.numeric(format(min(input_data$Date), "%Y"))
  end_year <- as.numeric(format(max(input_data$Date), "%Y"))
  mid_dates <- seq.Date(
    from = as.Date(paste0(start_year, "-07-01")),
    to = as.Date(paste0(end_year, "-07-01")),
    by = "1 year"
  )

  # Plot the effective precipitation
  plot1 <- ggplot2::ggplot(input_data, ggplot2::aes(x = Date, y = eff_Precip_cm)) +
    ggplot2::geom_col(color = "blue") +
    ggplot2::scale_x_date(
      name = "Date",
      breaks = mid_dates,
      labels = format(mid_dates, "%Y"),
      expand = c(0.004, 0.004)
    ) +
    ggplot2::scale_y_continuous(quote("Effective Precipitation (cm)"),
                       expand = c(0.004, 0.004)) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, size = 18, color = "black"),
      axis.text.y = ggplot2::element_text(size = 18, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.8, "cm"),
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      axis.title.y = ggplot2::element_text(size = 18, colour = "black"),
      plot.title = ggplot2::element_text(hjust = 0.01, vjust = -8, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  # Print the plot to display automatically
  print(plot1)
  return(input_data)
}
