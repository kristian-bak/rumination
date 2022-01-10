#' Convert NA to zero
#' @param data data.frame
#' 
convert_na_to_zero <- function(data) {
  
  num_col <- sapply(data, is.numeric)
  
  data[, num_col]
  
  from_na_to_zero <- function(x) {
    
    ifelse(is.na(x), 0, x)
    
  }
  
  data[, num_col] <- sapply(data[, num_col], from_na_to_zero)
  
  return(data)
  
}