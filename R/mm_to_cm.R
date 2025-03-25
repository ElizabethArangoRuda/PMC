#' Function to convert a variable from millimeters (mm) to centimeters (cm)
#'
#' @param input.data A dataframe that contains columns 'Date', 'Tmin', 'Tmax', 'PPT'
#' @param columns Selected column by the user such as 'PET' and/or 'PPT'.
#' @return A dataframe with original columns, except for the selected columns converted to centimeters.
#' @export
#' @examples
#' mm_to_cm(input_data, columns = c("PPT"))
mm_to_cm <- function(input_data, columns){
  input_data <- dplyr::mutate(input_data, across(all_of(columns), ~ . * 0.1, .names = "{.col}_cm"))
  return(input_data)
}
