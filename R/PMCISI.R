#' Peat Spread Rate Index (PMC_ISI)
#'
#' The Peat Spread Rate Index (PMC_ISI) integrates the Peat Moisture Code (PMC) and the Initial Spread Index (ISI)
#' to estimate the potential for above-ground fire spread in peatland environments. This combined index provides
#' a more representative measure of fire risk in peat fuels by accounting for both moisture conditions and surface fire behavior.
#'
#' @param input_data A dataframe that must include the columns \code{Date}, \code{PMC}, and \code{ISI}.
#' @param year_to_plot Optional. Specify a year (as a string or numeric) to generate a plot of PMC_ISI values for that year.
#'
#' @return A dataframe containing all original columns along with a new column \code{PMC_ISI}, representing the calculated peat spread index.
#' If \code{year_to_plot} is provided, a plot will be generated for visual interpretation.
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Date = as.Date(c("2014-01-01", "2014-01-02", "2014-01-03")),
#'   PMC = c(10.005600, 9.686799, 9.688504),
#'   ISI = c(0.2, 0.1, 0.1)
#' )
#' PMCISI(input_data, year_to_plot = "2014")
PMCISI <- function(input_data, year_to_plot=NULL){

  input_data <- dplyr::mutate(input_data,
                              PMC_ISI = PMC*ISI,
                              PMC_ISI_scaled = PMC*ISI/12) # Calculate PMCi as PMC * ISI / 12 so it roughly scales to between 10 and 100

  # Visuals
  if (!is.null(year_to_plot)){
    input_data <-  dplyr::filter(input_data, lubridate::year(Date)== year_to_plot)
    message(paste("Plotting data for year:", year_to_plot))
  }

  plot <-
    ggplot2::ggplot(input_data, ggplot2::aes(x=Date, y=PMC_ISI_scaled)) +
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    ggplot2::scale_x_date(
      date_labels = "%b",        # Display month and day (e.g., "Jan 01")
      date_breaks = "1 month",         # Set the break interval to 1 day
      expand = c(0.004, 0.004)
    ) +
    ggplot2::labs(x = "Date", y = expression(PMC[ISI]))+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, size = 14, hjust = -0.5, color = "black"),
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

  # ---- Display Combined Plot ----
  print(plot)

  # ---- Return PMCi data ----
  return(input_data)

}
