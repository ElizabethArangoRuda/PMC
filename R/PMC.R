#' Peat Moisture Code (PMC)
#'
#' The Peat Moisture Code (PMC) is calculated using a daily bookkeeping approach that integrates actual evapotranspiration and effective precipitation to simulate peatland moisture dynamics over time.
#'
#' PMC is calculated using the following formula:
#' \deqn{PMC_i = PMC_{i-1} + \frac{AET_i - EffPrecip_i}{Sy}}{PMC[i] = PMC[i-1] + ((AET[i] - EffPrecip[i]) / Sy)}
#'
#' Where:
#' \itemize{
#'   \item \eqn{PMC_i} is the PMC value on day \eqn{i},
#'   \item \eqn{PMC_{i-1}} is the PMC of the previous day (or the initial start-up value if \eqn{i = 1}),
#'   \item \eqn{AET_i} is actual evapotranspiration (cm) on day \eqn{i},
#'   \item \eqn{EffPrecip_i} is effective precipitation (cm) on day \eqn{i},
#'   \item \eqn{Sy} is the specific yield, dynamically calculated based on \eqn{PMC_{i-1}}.
#' }
#'
#' Specific yield (\eqn{Sy}) represents the ratio of water table change due to water input or loss in the peat system. It ranges between 0 and 1 and is modeled using an exponential decay function:
#' \deqn{Sy = A \cdot \exp(-B \cdot PMC_{i-1})}{Sy = A * exp(-B * PMC[i - 1])}
#'
#' To prevent unrealistic changes in storage, a minimum \code{Sy} value of 0.1 is enforced. A maximum multiplier of 10 is also applied to avoid overestimation of recharge.
#'
#' The parameters \code{A=0.8674} and \code{B=0.0540} are derived from an exponential fit between specific yield and depth below ground (in cm) using field data from bogs and treed poor fens in boreal Alberta. These can be modified to calibrate the model to specific regions or peatland types.
#'
#' @param input_data A data frame that includes at least 'Date', the selected PET column (in cm), and 'eff_Precip_cm'.
#' @param PET_column Name of the column containing daily potential evapotranspiration values (in centimeters).
#' @param A Parameter from the exponential Sy-depth relationship. Default is 0.8674.
#' @param B Parameter from the exponential Sy-depth relationship. Default is 0.0540.
#' @param start_PMC Initial PMC value (i.e., initial water table depth in cm).
#' @param C Lower limit for PET, constrained to a minimum of 0.15 to account for evapotranspiration from trees.
#' @param Sy_min Minimum allowable specific yield. Default is 0.1.
#' @param PMC_min Minimum allowable PMC value to cap extreme low estimates. Optional.
#' @param year_to_plot Optional. Numeric value indicating the year to display in a generated plot of daily PET.
#' @returns A data frame that retains the original input columns and adds a new column with the calculated PMC values.
#'
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   eff_Precip_cm = c(0.0, 0.21, 0.01),
#'   PET = c(0.0034, 0.0154, 0.0111)
#' )
#' PMC(input_data,
#'          PET_column = "PET",
#'          A = 0.8674,
#'          B = 0.0540,
#'          start_PMC = 10,
#'          C = 0.15,
#'          Sy_min = 0.1,
#'          PMC_min = -5,
#'          year_to_plot = "2024")

PMC <- function(input_data,
                     PET_column,
                     A,
                     B,
                     start_PMC,  # WTD = 10 cm is the starting point
                     C, # Limited to a minimum of 0.3 to account for ET from trees
                     Sy_min,
                     PMC_min,
                year_to_plot=NULL
                ) {

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
    #input_data$PET_multiplier[1] <- 1 - (0.0004 + (1 - C) / (1 + exp(-3.45 * (log(start_PMC) - 3.743))))
    input_data$PET_multiplier[1] <- 1 - ((1 - C) / (1 + exp(-3.45 * (log(start_PMC) - 3.743))))
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
      # PET_multiplier <- 1 - (0.0004 + (1 - C) /
      #                          (1 + exp(-3.45 * (log(prev_PMC) - 3.743))))
      PET_multiplier <- 1 - ((1 - C) /
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

  # Visuals
  if (!is.null(year_to_plot)){
    data_filtered <-  dplyr::filter(input_data, lubridate::year(Date)== year_to_plot)
    #message(paste("Plotting data for year:", year_to_plot))
  }

  start_year <- as.numeric(format(min(data_filtered$Date), "%Y"))
  end_year <- as.numeric(format(max(data_filtered$Date), "%Y"))
  mid_dates <- seq.Date(
    from = as.Date(paste0(start_year, "-07-01")),
    to = as.Date(paste0(end_year, "-07-01")),
    by = "1 year"
  )

  plot <- ggplot2::ggplot(data_filtered, ggplot2::aes(x=Date)) +
    ggplot2::geom_point(ggplot2::aes(y=PMC), color = "steelblue") +
    ggplot2::geom_line(ggplot2::aes(y=PMC), color = "steelblue") +
    ggplot2::scale_x_date(
      name = "Date",
      breaks = mid_dates,
      labels = format(mid_dates, "%Y"),
      expand = c(0.004, 0.004)
    ) +
    ggplot2::scale_y_continuous(name = "PMC",
                       #limits = c(0, 120),
                       #breaks = seq(0, 120, by = 20),
                       expand = c(0.004, 0.004)) +
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

  print(plot)

  remove_col <- c('Sy', 'AET_today', 'PET_multiplier')

  # Only remove columns that exist in input_data
  input_data <- dplyr::select(input_data, -dplyr::any_of(remove_col))

  message("PMC was calculated successfully. If your dataset includes ISI, you can now proceed with the PMC_ISI calculation using the PSI() function.")
  # Return the processed data
  return(input_data)
}

