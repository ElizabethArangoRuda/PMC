#' Pryestly-Taylor Potential Evapotranspiration
#'
#' @param input.data
#' @param latitude
#' @param alpha
#' @param y
#' @param Gsc
#' @param lambda
#' @param a
#' @param b
#'
#' @returns
#' @export
#'
#' @examples
Calculate_PET <- function(input.data,
                           latitude = 56.9002499,
                           alpha = 1, # Priestly-Taylor alpha constant
                           y = 0.063, #  Psychrometric constant (kPa/°C)
                           Gsc = 0.0820, # Solar constant in MJ m^-2 min^-1
                           lambda = 2453,
                           a = 0.11, # Constant in calculation of Rn from Kext
                           b = 0.59 # Constant in calculation of Rn from Kext
) {

  # Check if either Tmean exists or both Tmin and Tmax exist
  if (!"Tmean" %in% colnames(input.data) && (!"Tmin" %in% colnames(input.data) | !"Tmax" %in% colnames(input.data))) {
    stop("At least 'Tmean' or both 'Tmin' and 'Tmax' columns are required to proceed with the PET calculation.")
  }

  if ("latitude" %in% colnames(input.data)) {
    latitude <- input.data$latitude
  }

  if (!"Date" %in% colnames(input.data)) {
    stop("The 'Date' column is missing in the input data.")
  }

  # Check if meant temperature (Tmean) exists; if not, calculate it
  if (!"Tmean" %in% colnames(input.data)){
    input.data <- dplyr::mutate(input.data,
                                Tmean = (Tmin + Tmax) / 2 # If Tmean doesn't exist, calculate it
    )
  }

  # Convert latitude to radians
  input.data <- dplyr::mutate(input.data,
                              Date = as.Date(Date),
                              phi = latitude * pi / 180,  # Latitude in radians
                              Julian_Date = as.numeric(format(Date, "%j")),  # Extract day of the year


                              # Conditional lambda calculation based on the presence of Tmean
                              lambda = (2.501 - 0.00237 * Tmean)*1000, # Latent heat of vaporization (MJ/kg)
                              # Kext: Extra-terrestrial radiation
                              delta = 0.409 * sin((2 * pi / 365) * Julian_Date - 1.39), # Solar declination
                              ws = acos(-tan(phi) * tan(delta)), # Sunset hour angle
                              dr = 1 + 0.033 * cos((2 * pi / 365)* Julian_Date), # Inverse relative distance Earth-Sun
                              Kext =  ((24 * 60) / pi) * Gsc * dr * (ws * sin(phi) * sin(delta) + cos(phi) * cos(delta) * sin(ws))) #Extra terrestrial radiation

  # Check if Net Radiation (Rn) exist; if not, calculate it
  if (!"Rn" %in% colnames(input.data)){
    input.data <- dplyr::mutate(input.data,
                                Sw = a * Kext * (Tmax - Tmin)^b, # MJ m^-2 day^-1,
                                Rn= Sw*0.9
    )
  }

  input.data <- dplyr::mutate(input.data,
                              # Priestly-Taylor ET
                              slope = 0.61365 * (17.502 / (240.97 + Tmean) - 17.502 * Tmean / ((240.97 + Tmean)^2)) * exp(17.502 * Tmean / (240.97 + Tmean)), # Slope of the saturation vapor pressure curve (kPa/°C)
                              PET_Calculated = alpha * (slope / ((slope + y) * lambda)) * Rn * 100) # Daily ET in mm/day


  ## Plot daily ET
  plot1 <- ggplot2::ggplot(input.data, ggplot2::aes(x=Date)) +
    ggplot2::geom_line(ggplot2::aes(y=PET_Calculated), color = "blue") +
    ggplot2::scale_x_date(
      name = "Date",
      date_breaks = "year",
      labels = scales::date_format("%Y"),
      expand = c(0.004, 0.004)) +
    ggplot2::scale_y_continuous(name = "PET (cm)",
                       #limits = c(0, 1),
                       #breaks = seq(0, 1, by = 0.2),
                       expand = c(0.001, 0.004)) +
    ggplot2::labs(title="Priestly-Taylor Potential Evapotranspiration")+
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
      plot.title = ggplot2::element_text(hjust = 0.5, vjust = 1.5, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  # Print the plot to display automatically
  print(plot1)

  return(input.data)
}
