#' Clean and Validate Input Data
#'
#' This function prepares and validates the initial input dataset by ensuring it contains the necessary columns and correct data types. It is recommended to run this function before using any other functions in the package to ensure consistent formatting.
#'
#' @param input_data A data frame that must include the following columns: \code{Date}, \code{Tmin}, \code{Tmax}, and \code{PPT}.
#'
#' @return A cleaned data frame with:
#' \itemize{
#'   \item \code{Date} converted to class \code{Date} (if not already),
#'   \item all other columns converted to numeric,
#'   \item optional message if required columns (e.g., \code{PET}) are missing.
#' }
#'
#' @details
#' This function verifies that all required columns are present and that data types are correctly formatted. Specifically, it ensures the \code{Date} column is of class \code{Date}, and converts all other columns to numeric as needed. Any additional columns in the input data frame will be preserved unless otherwise specified.
#'
#' If the \code{PET} column is missing, a message will notify the user that it can be calculated using the \code{Calculate_PET()} function.
#'
#' If missing values are detected in the dataset, the function will prompt the user and, if approved, interpolate them using the \code{na.approx()} function from the \code{zoo} package.
#'
#' @examples
#' input_data <- data.frame(
#'   Date = as.Date(c("2024-01-01", "2024-01-02", "2023-01-03")),
#'   Tmin = c(-36.1, -23.8, -25.6),
#'   Tmax = c(-27.0, -7.5, -12.4),
#'   PPT = c(0.0, 2.1, 0.1),
#'   ISI = c(1.1, 1.2, 1.5)
#' )
#' clean_data(input_data)
#'
#' @export
clean_data <- function(input_data) {

  # Validate input_data

  if (!is.data.frame(input_data)) {
    stop("Error: input_data must be a dataframe.")
  }

  required_cols <- c("Date", "Tmin", "Tmax", "PPT")

  if (!all(required_cols %in% colnames(input_data))){
    missing_cols <- required_cols[!required_cols %in% colnames(input_data)]
    stop(paste("Error: The following required columns are missing:", paste(missing_cols, collapse = ",")))
  }

  input_data <- input_data[, required_cols]

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
    message("The 'PET' column is missing. You can calculate it using the 'Calculate_PET()' function.")
  }

  # Check for missing values (excluding Date column)
  missing_count <- sum(is.na(input_data[-which(names(input_data) == "Date")]))
  input_data <- dplyr::mutate(input_data, dplyr::across(.cols = -Date, as.numeric))
  if (missing_count > 0){
    cat("\n Warning: Your dataset contains", missing_count, "missing values.\n")
    cat("Do you want to interpolate them?\n")
    cat(" 1: Yes, interpolate\n")
    cat(" 2: No, keep the missing values\n")

    user_choice <- readline(prompt = "Enter your choice (1 or 2): ")

    if (user_choice == "1"){
      input_data <- dplyr::mutate(input_data, dplyr::across(.cols = -Date, .fns = ~ zoo::na.approx(., na.rm=FALSE)
                                                            ))
      message("Missing values have been interpolated.")
    } else {
      message("Missing values were not interpolated.")
    }
  } else {
    message("No missing values found.")
  }

  message("Note: 'PPT' and 'PET' values must be in centimeters (cm). If your data is in millimeters (mm), please use the 'mm_to_cm()' function to convert it.")
  message("Data processed successfully with ", nrow(input_data), " rows and ", ncol(input_data), " columns.")

  # Return the processed dataframe
  return(input_data)
}
