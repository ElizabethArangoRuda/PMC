# Adapt this function so it works for any variable that the user add
mm_to_cm <- function(input.data, columns){
  input.data <- input.data %>%
    dplyr::mutate(across(all_of(columns), ~ . * 0.1, .names = "{.col}_cm"))
  return(input.data)
}
