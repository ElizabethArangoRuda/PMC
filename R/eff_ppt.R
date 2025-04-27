#' Calculate Effective Precipitation
#'
#' Computes the effective precipitation contributing to peatland water storage, based on a user-defined threshold.
#' Effective precipitation reflects the portion of daily precipitation that exceeds a minimum value necessary to impact
#' hydrological processes in peatlands.
#'
#' @param input_data A dataframe containing at least a 'Date' column and a daily precipitation column in centimeters.
#' @param threshold Numeric. The minimum precipitation threshold (in cm) required before water contributes to storage.
#' This value is **user-dependent** and may vary by site or model assumptions. Default is 0.1 cm.
#' @param column Character. The name of the column containing daily precipitation values (e.g., "PPT_cm").
#' @param year_to_plot Optional. Numeric value indicating the year to display in a generated plot of daily PET.
#' @return A dataframe including all original columns, along with a new column named \code{Eff_Precip}
#' representing the effective precipitation values for each day.
#'
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Date = as.Date(c("2024-01-01", "2024-01-02", "2023-01-03")),
#'   Tmin = c(-36.1, -23.8, -25.6),
#'   Tmax = c(-27.0, -7.5, -12.4),
#'   PPT_cm = c(0.0, 2.1, 0.1)
#' )
#' eff_ppt(input_data, column = "PPT_cm", threshold = 0.1, year_to_plot = c(2014,2016))

eff_ppt <- function(input_data, column, threshold, year_to_plot=NULL){

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



  # Visuals
  if (!is.null(year_to_plot)){
    data_filtered <-  dplyr::filter(input_data, lubridate::year(Date)== year_to_plot)
    message(paste("Plotting data for year:", year_to_plot))
  }

  start_year <- as.numeric(format(min(data_filtered$Date), "%Y"))
  end_year <- as.numeric(format(max(data_filtered$Date), "%Y"))
  mid_dates <- seq.Date(
    from = as.Date(paste0(start_year, "-07-01")),
    to = as.Date(paste0(end_year, "-07-01")),
    by = "1 year"
  )


  # Plot the effective precipitation
  plot1 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = Date, y = eff_Precip_cm)) +
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
      axis.text.x = ggplot2::element_text(angle = 0, size = 14, color = "black"),
      axis.text.y = ggplot2::element_text(size = 14, colour = "black"),
      axis.title.x = ggplot2::element_blank(),
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.key.size = ggplot2::unit(0.8, "cm"),
      legend.key.width = ggplot2::unit(1, "cm"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      axis.title.y = ggplot2::element_text(size = 14, colour = "black"),
      plot.title = ggplot2::element_text(hjust = 0.01, vjust = -8, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  # Print the plot to display automatically
  print(plot1)
  message("Effective precipitation was calculated using a threshold of ", threshold)
  message("You can now compute potential evapotranspiration using the `PETpt()` function.")
  return(input_data)
}
