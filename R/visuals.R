#' Multi-panel Plot of Selected Variables
#'
#' This function creates a four-panel plot to visualize temporal trends in selected variables from a given dataset.
#' It is useful for comparing key hydrometeorological and modeled parameters over time.
#'
#' @param input_data A dataframe containing a 'Date' column and the variables to be plotted.
#' @param var1 The name of the variable to be plotted in the first panel (as a string).
#' @param var2 The name of the variable to be plotted in the second panel (as a string).
#' @param var3 The name of the variable to be plotted in the third panel (as a string).
#' @param var4 The name of the variable to be plotted in the fourth panel (as a string).
#' @param x_label_vr1 The y-axis label for the first variable.
#' @param x_label_vr2 The y-axis label for the second variable.
#' @param x_label_vr3 The y-axis label for the third variable.
#' @param x_label_vr4 The y-axis label for the fourth variable.
#' @param year_to_plot Optional. Numeric value indicating the year to display in a generated plot of daily PET.
#'
#' @return A combined figure containing four ggplot2 panels arranged vertically, each showing the time series of one variable.
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   PET = c(0.0034, 0.0154, 0.0111),
#'   Tavg = c(-31.55, -15.65, -19.00),
#'   PMC = c(10.01, 9.69, 9.69),
#'   eff_Precip_cm = c(0.00, 0.21, 0.01)
#' )
#' visuals(
#'   input_data,
#'   var1 = "PET",
#'   var2 = "Tavg",
#'   var3 = "PMC",
#'   var4 = "eff_Precip_cm",
#'   x_label_vr1 = "PET (cm)",
#'   x_label_vr2 = "Tair (°C)",
#'   x_label_vr3 = "EffPrecipi (cm)",
#'   x_label_vr4 = "PMC",
#'   year_to_plot = "2024"
#' )
visuals <- function(input_data, var1, var2, var3, var4,
                    x_label_vr1,
                    x_label_vr2,
                    x_label_vr3,
                    x_label_vr4,
                    year_to_plot=NULL){


  # Visuals
  if (!is.null(year_to_plot)){
    data_filtered <-  dplyr::filter(input_data, lubridate::year(Date) %in% year_to_plot)
    #message(paste("Plotting data for year:", year_to_plot))
  }

  start_year <- as.numeric(format(min(data_filtered$Date), "%Y"))
  end_year <- as.numeric(format(max(data_filtered$Date), "%Y"))
  mid_dates <- seq.Date(
    from = as.Date(paste0(start_year, "-07-01")),
    to = as.Date(paste0(end_year, "-07-01")),
    by = "1 year"
  )

  P1 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[var1]]), color = "steelblue") + # Uses extraction operator
    ggplot2::scale_x_date(
      name = "Date",
      breaks = mid_dates,
      labels = scales::date_format("%Y"),
      expand = c(0.004, 0.004)
    ) +
    ggplot2::scale_y_continuous(name = x_label_vr1,
                       #limits = c(0, 1),
                       #breaks = seq(0, 1, by = 0.2),
                       expand = c(0.001, 0.004)) +
    ggplot2::labs(title="a")+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, size = 10, color = "black"),
      axis.text.y = ggplot2::element_text(size = 10, colour = "black"),
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

  P1

  P2 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[var2]]), color = "steelblue") +
    ggplot2::scale_x_date(
      name = "Date",
      breaks = mid_dates,
      labels = scales::date_format("%Y"),
      expand = c(0.004, 0.004)
    ) +
    ggplot2::scale_y_continuous(name = x_label_vr2,
                       #limits = c(0, 1),
                       #breaks = seq(0, 1, by = 0.2),
                       expand = c(0.004, 0.004)) +
    ggplot2::labs(title="b")+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, size = 10, color = "black"),
      axis.text.y = ggplot2::element_text(size = 10, colour = "black"),
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

  P2

  # Plot the effective precipitation
  P3 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = Date)) +
    ggplot2::geom_col(color = "blue", ggplot2::aes(y = .data[[var4]])) +
    ggplot2::scale_x_date(
      name = "Date",
      breaks = mid_dates,
      labels = scales::date_format("%Y"),  # Explicitly call `scales::date_format()`
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(name = x_label_vr4,
                                #limits = c(0, 100),
                                #breaks = seq(0, 100, by = 20),
                                expand = c(0.004, 0.004)) +
    ggplot2::labs(title="d")+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, size = 10, color = "black"),
      axis.text.y = ggplot2::element_text(size = 10, colour = "black"),
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

  P3

  P4 <- ggplot2::ggplot(data_filtered, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[var3]]), color = "steelblue") +
    ggplot2::scale_x_date(
      name = "Date",
      breaks = mid_dates,
      labels = scales::date_format("%Y"),
      expand = c(0.004, 0.004)
    ) +
    ggplot2::scale_y_continuous(name = x_label_vr3,
                       #limits = c(0, 100),
                       #breaks = seq(0, 100, by = 20),
                       expand = c(0.004, 0.004)) +
    ggplot2::labs(title="c")+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, size = 10, color = "black"),
      axis.text.y = ggplot2::element_text(size = 10, colour = "black"),
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

  P4

  plotgrid <- patchwork::wrap_plots(P1, P2, P3, P4, ncol = 1)

  print(plotgrid)

  return(input_data)
}
