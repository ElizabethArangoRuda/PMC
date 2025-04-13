#' Function to convert a variable from millimeters (mm) to centimeters (cm)
#'
#' @param input_data A dataframe that contains columns 'Date', 'Tmin', 'Tmax', 'PPT'
#' @param columns Selected column by the user such as 'PET' and/or 'PPT'.
#' @return A dataframe with original columns, except for the selected columns converted to centimeters.
#' @export
#' @examples
#' input_data <- data.frame(Date = as.Date(c("2024-01-01", "2024-01-02",
#' "2023-01-03")), Tmin = c(-36.1, -23.8, -25.6), Tmax = c(-27.0, -7.5, -12.4),
#' PPT = c(0.0, 2.1, 0.1))
#' mm_to_cm(input_data, columns = c("PPT"))
mm_to_cm <- function(input_data, columns){
  input_data <- dplyr::mutate(input_data, dplyr::across(dplyr::all_of(columns), ~ . * 0.1, .names = "{.col}_cm"))
  message("The following column(s) were successfully converted from millimeters to centimeters: ",
          paste(columns, collapse = ", "))
  message("Use mm_to_cm() function to convert PPT from mm to cm")
  return(input_data)
}
