#' Convert Millimeters to Centimeters
#'
#' This function converts specified variables in a dataframe from millimeters (mm) to centimeters (cm) by multiplying the values by 0.1.
#'
#' @param input_data A dataframe that includes at least a 'Date' column and one or more numeric columns (e.g., 'PPT', 'PET').
#' @param columns A character vector indicating which column(s) to convert from mm to cm (e.g., \code{c("PPT", "PET")}).
#'
#' @return A dataframe containing all original columns, with the specified column(s) converted to centimeters and renamed with a \code{_cm} suffix.
#'
#' @details This function preserves all columns in the original dataframe. The converted columns are added as new columns with names in the format \code{originalname_cm}.
#'
#' @export
#'
#' @examples
#' input_data <- data.frame(
#'   Date = as.Date(c("2024-01-01", "2024-01-02", "2023-01-03")),
#'   Tmin = c(-36.1, -23.8, -25.6),
#'   Tmax = c(-27.0, -7.5, -12.4),
#'   PPT = c(0.0, 2.1, 0.1)
#' )
#' mm_to_cm(input_data, columns = c("PPT"))
mm_to_cm <- function(input_data, columns){
  # Check if input_data is a dataframe
  if (!is.data.frame(input_data)) {
    stop("Error: 'input_data' must be a dataframe.")
  }

  # Check if specified columns exist in the dataframe
  missing_cols <- setdiff(columns, names(input_data))
  if (length(missing_cols) > 0) {
    stop(paste("The following column(s) are not found in the dataframe:",
               paste(missing_cols, collapse = ", ")))
  }
  input_data <- dplyr::mutate(input_data, dplyr::across(dplyr::all_of(columns), ~ . * 0.1, .names = "{.col}_cm"))
  message("The following column(s) were successfully converted from millimeters to centimeters: ",
          paste(columns, collapse = ", "))
  message("You can now compute effective precipitation using the `eff_ppt()` function.")
  return(input_data)
}
