#' Get patients
#' @param data tibble with Deltager column
#' 
get_patients <- function(data) {
  
  data %>% 
    dplyr::pull(Deltager) %>% 
    unique() %>% 
    sort()
  
}