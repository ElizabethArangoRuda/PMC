#' Clean the initial dataframe
#'
#' @param input_data A dataframe that contains columns 'Date', 'Tmin', 'Tmax', 'PPT'
#' @return A dataframe with date columns of class Date, and the other variables as numeric
#' @export
#' @examples
#' input_data <- data.frame(Date = as.Date(c("2024-01-01", "2024-01-02",
#' "2023-01-03")), Tmin = c(-36.1, -23.8, -25.6), Tmax = c(-27.0, -7.5, -12.4),
#' PPT = c(0.0, 2.1, 0.1))
#' cleaning_data(input_data)

cleaning_data <- function(input_data) {

  required_packages <- c("cowplot", "tidyverse", "readxl")

  purrr::walk(required_packages, function(pkg){
    if (!requireNamespace(pkg, quietly = TRUE)){
      stop(paste("Error: Package", pkg, "is not installed. Please install it using install.packages('", pkg, "')"))
    }
  })

  # Validate input_data

  if (!is.data.frame(input_data)) {
    stop("Error: input_data must be a dataframe.")
  }

  # Check for "Date" Column
  if (!"Date" %in% colnames(input_data)) {
    stop("Error: The dataset must contain a 'Date' column formatted as 'YYYY-MM-DD'.")
  }

  # Convert Date column to Date format if necessary
  if (!inherits(input_data$Date, "Date")) {
    input_data <- dplyr::mutate(input_data, Date = as.Date(Date))
  }

  # Convert numeric columns to numeric format
  input_data <- dplyr::mutate(input_data, dplyr::across(-dplyr::any_of("Date"), as.numeric))

  # Check if PET column exists and provide a message if not
  if (!"PET" %in% colnames(input_data)) {
    message("The 'PET' column is missing. You can calculate it using the 'PET_ESTIMATION()' function.")
  }

  message("Data processed successfully with ", nrow(input_data), " rows and ", ncol(input_data), " columns.")

  # Return the processed dataframe
  return(input_data)
}
