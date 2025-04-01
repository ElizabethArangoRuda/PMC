#' Plots of selected parameters
#'
#' @param input_data A dataframe containing the variables to be plotted.
#' @param var1 The variable to be plotted in the first panel.
#' @param var2 The variable to be plotted in the second panel.
#' @param var3 The variable to be plotted in the third panel.
#' @param var4 The variable to be plotted in the fourth panel.
#' @param x_label_vr1 The y-axis label for the first variable.
#' @param x_label_vr2 The y-axis label for the second variable.
#' @param x_label_vr3 The y-axis label for the third variable.
#' @param x_label_vr4 The y-axis label for the fourth variable.
#'
#' @return An arrangement of multiple plots as a single, combined figure.
#' @export
#'
#' @examples
#' input_data <- data.frame(Date = as.Date(c("2024-01-01", "2024-01-02",
#' "2023-01-03")), PET_Calculated = c(0.003444420, 0.015362293, 0.011119636),
#' Tmean = c(-31.55, -15.65, -19.00), PMC = c(10.005600, 9.686799, 9.688504),
#' eff_Precip_cm = c(0.00, 0.21, 0.01))
#' visuals(input_data,
#' var1="PET_Calculated",
#' var2="Tmean",
#' var3="PMC",
#' var4="eff_Precip_cm",
#' x_label_vr1 = "PET (cm)",
#' x_label_vr2 = "Tair (°C)",
#' x_label_vr3 = "PMC",
#' x_label_vr4 = "Eff PPT")
visuals <- function(input_data, var1, var2, var3, var4,
                    x_label_vr1,
                    x_label_vr2,
                    x_label_vr3,
                    x_label_vr4){

  P1 <- ggplot2::ggplot(input_data, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[var1]]), color = "steelblue") + # Uses extraction operator
    ggplot2::scale_x_date(
      name = "Date",
      date_breaks = "year",
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
      axis.text.x = ggplot2::element_text(angle = 0, size = 18, hjust = -2, color = "black"),
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
      plot.title = ggplot2::element_text(hjust = 0.01, vjust = -5, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  P1

  P2 <- ggplot2::ggplot(input_data, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[var2]]), color = "steelblue") +
    ggplot2::scale_x_date(
      name = "Date",
      date_breaks = "year",
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
      axis.text.x = ggplot2::element_text(angle = 0, size = 18, hjust = -2, color = "black"),
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
      plot.title = ggplot2::element_text(hjust = 0.01, vjust = -5, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  P2

  P3 <- ggplot2::ggplot(input_data, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y = .data[[var3]]), color = "steelblue") +
    ggplot2::scale_x_date(
      name = "Date",
      date_breaks = "year",
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
      axis.text.x = ggplot2::element_text(angle = 0, size = 18, hjust = -2, color = "black"),
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
      plot.title = ggplot2::element_text(hjust = 0.01, vjust = -5, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  P3

  # Plot the effective precipitation
  P4 <- ggplot2::ggplot(input_data, ggplot2::aes(x = Date)) +
    ggplot2::geom_col(color = "blue", ggplot2::aes(y = .data[[var4]])) +
    ggplot2::scale_x_date(
      name = "Date",
      date_breaks = "1 year",
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
      axis.text.x = ggplot2::element_text(angle = 0, size = 18, hjust = -2, color = "black"),
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
      plot.title = ggplot2::element_text(hjust = 0.01, vjust = -5, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  P4

  plotgrid <- cowplot::plot_grid(P1, P2, P3, P4,
                                 #labels = c('A', 'B', 'C', 'D'),
                                 ncol = 1,
                                 align = 'v',
                                 axis='lr')

  print(plotgrid)

  return(input_data)
}
