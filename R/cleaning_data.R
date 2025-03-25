#' Clean the initial dataframe
#'
#' @param input_data A dataframe that contains columns 'Date', 'Tmin', 'Tmax', 'PPT'
#' @return A dataframe with date columns of class Date, and the other variables as numeric
#' @export
#' @examples
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

  # Convert the Date column to Date format
  if(!inherits(input_data$Date, "Date")){
    tryCatch({
      input_data <- dyplyr::mutate(input_data, as.Date(Date))
    }, error = function(e) {
      stop("Error: Unable to convert 'Date' column to Date format. Please ensure it follows 'YYYY-MM-DD' format.")
    })
  }

  # Convert other columns to numeric
  input_data <- dplyr::mutate(input_data, across(-any_of("Date"), ~suppressWarnings(as.numeric(.))))

  # Check if PET column exists and provide a message if not
  if (!"PET" %in% colnames(input_data)) {
    message("The 'PET' column is missing. You can calculate it using the 'PET_ESTIMATION()' function.")
  }

  message("Data processed successfully with ", nrow(input_data), " rows and ", ncol(input_data), " columns.")

  # Return the processed dataframe
  return(input_data)
}
