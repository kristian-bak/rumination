#' Replace NA with lag
#' @param x vector of any class
#' 
replace_na_with_lag <- function(x) {
  
  tidyr::fill(data = as.data.frame(x), x, .direction = "down") %>% 
    dplyr::pull(x)
  
}

#' Map option to function
#' @param x string ("0", "1", "2" or "3")
map_option_to_function <- function(x) {
  
  switch(x, 
         "1" = replace_na_with_lag, 
         "2" = replace_na_with_reg_line)
  
}

#' Prep cols
#' @param data tibble with questions
#' @param option string ("0" or "1")
#' 
prep_cols <- function(data, option) {
  
  if (option == "0") {
    return(data)
  }
  
  f <- map_option_to_function(option)
  
  if (option == "1") {
    data %>% 
      dplyr::group_by(Deltager) %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("Spørgsmål"), f))  
  } else if (option == "2") {
    data %>% 
      dplyr::group_by(Deltager) %>% 
      dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("Spørgsmål"), f, x = Uge))  
  } else {
    stop("option must be a string valued 0, 1 or 2")
  }
  
}