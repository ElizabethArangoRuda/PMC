#' PMC Calculation
#'
#' @param input_data A dataframe containing the columns 'Date', 'PET_cm', and 'eff_Precip_cm'.
#' @param PET_column A PET column is necessary for calculating PCM. The required unit of measurement is centimeters.
#' @param A Equation paramater
#' @param B Equation parameter
#' @param start_PMC A parameter to establish the initial value for the water table depth in centimeters.
#' @param C Limited to a minimum of 0.3 to account for ET from trees
#' @param Sy_min Minimum Specific Yield (Sy)
#' @param PMC_min Minimum PMC value
#'
#' @returns A dataframe that retains the original columns and adds a column with the calculated PMC.
#' @export
#'
#' @examples
#' input_data <- data.frame(Date = as.Date(c("2024-01-01", "2024-01-02",
#' "2023-01-03")), eff_Precip_cm = c(0.0, 0.21, 0.01), PET_Calculated =
#' c(0.003444420, 0.015362293, 0.011119636))
#' PMC_calc(input_data, PET_column = "PET_Calculated",
#' A = 0.8558,
#' B = 0.0337,
#' start_PMC = 10,
#' C = 0.1,
#' Sy_min = 0.1,
#' PMC_min = -5)

PMC_calc <- function(input_data,
                     PET_column,
                     A,
                     B,
                     start_PMC,  # WTD = 10 cm is the starting point
                     C, # Limited to a minimum of 0.3 to account for ET from trees
                     Sy_min,
                     PMC_min) {

  # Initialize new columns
  input_data$PMC <- NA
  input_data$Sy <- NA
  input_data$AET_today <- NA
  input_data$PET_multiplier <- NA

  # Set the initial PMC
  # input.data$PMC[1] <- start_PMC + (input.data[[PET_column]][1] *(1 - (0.0004 + (1 - C) /
  #                                                                        (1 + exp(-3.45 * (log(start_PMC) - 3.743))))) - input.data$eff_Precip_cm[1])/ (A*exp(-B*(start_PMC)))

  input_data$Sy[1] = (A*exp(-B*(start_PMC)))
  if (input_data$Sy[1] < Sy_min) {
    input_data$Sy[1] = Sy_min
  } else if (input_data$Sy[1] > 1){
    input_data$Sy[1] <- 1
  }

  # Determine PET_multiplier
  if (start_PMC <= 0) { # Calculated PMC if the previous day had a PMC of 0 or negative (e.g., WT at or above the surface) by setting Sy to 1 due to the ponding. Sy is also set to 1 so it does not become more than 1.
    input.data$PET_multiplier[1] <- 1 # PET multiplier at the surface is 1 (full potential evapotranspiration is occurring). That is, when WT above the surface (i.e., soil is saturated - no water limitation).
  } else {
    input_data$PET_multiplier[1] <- 1 - (0.0004 + (1 - C) / (1 + exp(-3.45 * (log(start_PMC) - 3.743))))
  }

  input_data$PMC[1] <- start_PMC + ((input_data[[PET_column]][1]*input_data$PET_multiplier[1] - input_data$eff_Precip_cm[1])/input_data$Sy[1])

  for (i in 2:nrow(input_data)) {

    # Gather previous PMC and inputs
    prev_PMC <- input_data$PMC[i - 1]
    pet_today <- input_data[[PET_column]][i]
    eff_precip_today <- input_data$eff_Precip_cm[i]

    # Skip iteration for NA values
    if (is.na(prev_PMC) || is.na(pet_today) || is.na(eff_precip_today)) {
      next
    }

    Sy = (A*exp(-B*(prev_PMC)))
    if (Sy < Sy_min) {
      Sy = Sy_min
    } else if (Sy > 1){
      Sy <- 1
    }

    # Determine PET_multiplier
    if (prev_PMC <= 0) { # Calculated PMC if the previous day had a PMC of 0 or negative (e.g., WT at or above the surface) by setting Sy to 1 due to the ponding. Sy is also set to 1 so it does not become more than 1.
      PET_multiplier <- 1 # PET multiplier at the surface is 1 (full potential evapotranspiration is occurring). That is, when WT above the surface (i.e., soil is saturated - no water limitation).
    } else {
      PET_multiplier <- 1 - (0.0004 + (1 - C) /
                               (1 + exp(-3.45 * (log(prev_PMC) - 3.743))))
    }

    # Compute AET_today
    AET_today <- input_data[[PET_column]][i] * PET_multiplier


    # Calculate today's PMC
    pmc_today <- prev_PMC + ((AET_today - input_data$eff_Precip_cm[i]) / Sy)

    #Apply ponding or minimum PMC limits if necessary
    if (pmc_today < PMC_min) {
      pmc_today <- PMC_min # If statement that sets the PMC[i] to the minimum PMC value (set above) if the PMC calculated above is lower than the minimum

    }

    # Store computed values
    input_data$PMC[i] <- pmc_today
    input_data$Sy[i] <- Sy
    input_data$PET_multiplier[i] <- PET_multiplier
    input_data$AET_today[i] <- AET_today
  }


  plot <- ggplot2::ggplot(input_data, ggplot2::aes(x=Date)) +
    ggplot2::geom_point(ggplot2::aes(y=PMC), color = "steelblue") +
    ggplot2::geom_line(ggplot2::aes(y=PMC), color = "steelblue") +
    ggplot2::scale_x_date(
      name = "Date",
      date_breaks = "year",
      labels = scales::date_format("%Y"),
      expand = c(0.004, 0.004)
    ) +
    ggplot2::scale_y_continuous(name = "PMC",
                       #limits = c(0, 120),
                       #breaks = seq(0, 120, by = 20),
                       expand = c(0.004, 0.004)) +
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
      plot.title = ggplot2::element_text(hjust = 0.01, vjust = -8, size = 18),
      plot.margin = ggplot2::margin(0.1, 0.2, 0.1, 0.2, "cm")
    )

  print(plot)

  # Return the processed data
  return(input_data)
}

