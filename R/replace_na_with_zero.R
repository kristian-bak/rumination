#' Replace NA with zero
#' @param data data.frame
#' 
replace_na_with_zero <- function(data) {
  
  num_col <- sapply(data, is.numeric)
  
  from_na_to_zero <- function(x) {
    
    ifelse(is.na(x), 0, x)
    
  }
  
  data[, num_col] <- sapply(data[, num_col], from_na_to_zero)
  
  return(data)
  
}