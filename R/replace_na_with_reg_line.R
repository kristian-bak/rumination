#' Replace NA with regression line
#' @param x numeric vector
#' @param y numeric vector
replace_na_with_reg_line <- function(x, y) {
  
  if (is.na(y[1])) {
    return(y)
  }
  
  id_na <- y %>% 
    is.na() %>% 
    which() %>% 
    purrr::pluck(1)
  
  id_na_prior <- id_na - 1
  id_na_post <- id_na + 1
  
  x_diff <- id_na_post - id_na_prior ## always 2: for now only supporting 1 NA replacment
  
  interpolate_slope <- (y[id_na_post] - y[id_na_prior]) / x_diff
  interpolate_intercept <- y[id_na_prior]
  interpolate_value <- interpolate_intercept + interpolate_slope * 1 ## For now NA value will always be +1 the intercept
  
  y[id_na] <- interpolate_value
  
  return(y)
  
}