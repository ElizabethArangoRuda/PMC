#' Priestley–Taylor Potential Evapotranspiration (PET)
#'
#' This function estimates daily potential evapotranspiration (PET) using the Priestley–Taylor (PT) method, a simplified
#' alternative to the Penman-Monteith equation. The PT method combines radiation and temperature-based components into a
#' single formulation using an empirical coefficient, \eqn{\alpha}{α}.
#'
#' PET is computed as:
#'
#' \deqn{ET_{pt} = \alpha \cdot \frac{\Delta \cdot (R_n - G)}{\lambda_v \cdot (\Delta + \gamma)} \cdot 100}{
#' ETpt = α * [Δ * (Rn - G)] / [λv * (Δ + γ)] * 100
#' }
#'
#' Where:
#' \itemize{
#'   \item \eqn{\alpha}{α} is the Priestley–Taylor coefficient (dimensionless),
#'   \item \eqn{\Delta}{Δ} is the slope of the saturation vapor pressure curve (kPa/°C),
#'   \item \eqn{R_n} is the net radiation (MJ/m²/day),
#'   \item \eqn{G} is the soil heat flux, which is assumed to be negligible in daily time steps,
#'   \item \eqn{\lambda_v}{λv} is the latent heat of vaporization (MJ/kg),
#'   \item \eqn{\gamma} is the psychrometric constant (kPa/°C).
#' }
#'
#' The resulting PET values are expressed in centimeters (cm) per day.
#'
#' @param input_data A data frame containing at least the following columns: \code{Date}, \code{Tmin}, and \code{Tmax}. The optional \code{Tavg} column will be calculated if not provided.
#' @param latitude Numeric. Latitude of the study location in decimal degrees. **User-dependent** and essential for calculating extraterrestrial radiation.
#' @param alpha Numeric. Priestley–Taylor coefficient. **User-dependent**; typically ranges from 1.0 to 1.26 depending on surface conditions. Default is 1.
#' @param y Numeric. Psychrometric constant (kPa/°C). Default is 0.063. This parameter may vary depending on elevation and air pressure conditions.
#' @param Gsc Numeric. Solar constant (MJ m\eqn{^{-2}} min\eqn{^{-1}}). Default is 0.0820.
#' @param lambda Numeric. Latent heat of vaporization (MJ/kg). Default is 2453. If not provided, it will be internally estimated based on mean air temperature.
#' @param a Numeric. Empirical coefficient for calculating net radiation from extraterrestrial radiation. Default is 0.17.
#' @param b Numeric. Empirical coefficient for net radiation estimation. Default is 0.59.
#' @param year_to_plot Optional. Numeric value indicating the year to display in a generated plot of daily PET.
#'
#' @return A data frame with the original columns plus a new column \code{PET_Calculated}, representing daily potential evapotranspiration in centimeters (cm).
#'
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Date = as.Date(c("2024-01-01", "2024-01-02", "2023-01-03")),
#'   Tmin = c(-36.1, -23.8, -25.6),
#'   Tmax = c(-27.0, -7.5, -12.4)
#' )
#'
#' Calculate_PET(
#'   input_data,
#'   latitude = 56.9002499,
#'   alpha = 1,
#'   y = 0.063,
#'   Gsc = 0.0820,
#'   lambda = 2453,
#'   a = 0.17,
#'   b = 0.59
#' )
Calculate_PET <- function(input_data,
                           latitude,
                           alpha,
                           y,
                           Gsc,
                           lambda,
                           a,
                           b,
                          year_to_plot=NULL
) {

  # Check if either Tavg exists or both Tmin and Tmax exist
  if (!"Tavg" %in% colnames(input_data) && (!"Tmin" %in% colnames(input_data) | !"Tmax" %in% colnames(input_data))) {
    stop("At least 'Tavg' or both 'Tmin' and 'Tmax' columns are required to proceed with the PET calculation.")
  }

  if ("latitude" %in% colnames(input_data)) {
    latitude <- input_data$latitude
  }

  if (!"Date" %in% colnames(input_data)) {
    stop("The 'Date' column is missing in the input data.")
  }

  # Check if meant temperature (Tavg) exists; if not, calculate it
  if (!"Tavg" %in% colnames(input_data)) {
    # If Tavg doesn't exist, calculate it from Tmin and Tmax
    input_data <- dplyr::mutate(input_data, Tavg = (Tmin + Tmax) / 2)

  } else {
    # If Tavg exists but has missing values, fill them
    input_data <- dplyr::mutate(input_data,
                                Tavg = ifelse(is.na(Tavg), (Tmin + Tmax) / 2, Tavg)
    )
  }

  # Convert latitude to radians
  input_data <- dplyr::mutate(input_data,
                              Date = as.Date(Date),
                              phi = latitude * pi / 180,  # Latitude in radians
                              Julian_Date = as.numeric(format(Date, "%j")),  # Extract day of the year


                              # Conditional lambda calculation based on the presence of Tavg
                              lambda = (2.501 - 0.00237 * Tavg)*1000, # Latent heat of vaporization (MJ/kg)
                              # Kext: Extra-terrestrial radiation
                              delta = 0.409 * sin((2 * pi / 365) * Julian_Date - 1.39), # Solar declination
                              ws = acos(-tan(phi) * tan(delta)), # Sunset hour angle
                              dr = 1 + 0.033 * cos((2 * pi / 365)* Julian_Date), # Inverse relative distance Earth-Sun
                              Kext =  ((24 * 60) / pi) * Gsc * dr * (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))) #Extra terrestrial radiation

  # Check if Net Radiation (Rn) exist; if not, calculate it
  if (!"Rn" %in% colnames(input_data)){
    input_data <- dplyr::mutate(input_data,
                                Sw = a * Kext * (Tmax - Tmin)^b, # MJ m^-2 day^-1,
                                Rn= Sw*0.9
    )
  }

  input_data <- dplyr::mutate(input_data,
                              # Priestly-Taylor ET
                              slope = 0.61365 * (17.502 / (240.97 + Tavg) - 17.502 * Tavg / ((240.97 + Tavg)^2)) * exp(17.502 * Tavg / (240.97 + Tavg)), # Slope of the saturation vapor pressure curve (kPa/°C)
                              PET_Calculated = alpha * (slope / ((slope + y) * lambda)) * Rn * 100) # Daily ET in cm/day

  # Visuals
  if (!is.null(year_to_plot)){
    input_data <-  dplyr::filter(input_data, lubridate::year(Date)== year_to_plot)
    message(paste("Plotting data for year:", year_to_plot))
  }

  start_year <- as.numeric(format(min(input_data$Date), "%Y"))
  end_year <- as.numeric(format(max(input_data$Date), "%Y"))
  mid_dates <- seq.Date(
    from = as.Date(paste0(start_year, "-07-01")),
    to = as.Date(paste0(end_year, "-07-01")),
    by = "1 year"
  )



  ## Plot daily ET
  plot1 <- ggplot2::ggplot(input_data, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y=PET_Calculated), color = "blue") +
    ggplot2::scale_x_date(
      name = "Date",
      breaks = mid_dates,
      labels = format(mid_dates, "%Y"),
      expand = c(0.004, 0.004)
    ) +
    ggplot2::scale_y_continuous(name = "PET (cm)",
                       #limits = c(0, 1),
                       #breaks = seq(0, 1, by = 0.2),
                       expand = c(0.001, 0.001)) +
    ggplot2::labs(title="Priestly-Taylor Potential Evapotranspiration")+
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.text.x = ggplot2::element_text(angle = 0, size = 18, hjust = 0.5, color = "black"),
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
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 1.5, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  # Print the plot to display automatically
  print(plot1)

  remove_col <- c('phi', 'Julian_Date', 'lambda', 'delta', 'ws', 'dr', 'Kext', 'Sw', 'Rn', 'slope')

  # Only remove columns that exist in input_data
  input_data <- dplyr::select(input_data, -dplyr::any_of(remove_col))

  message("PET calculation completed successfully. You may now proceed with PMC estimation using the `PMC_calc()` function.")

  return(input_data)
}
