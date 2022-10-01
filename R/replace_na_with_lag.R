#' Replace NA with lag (for all numeric columns in data)
#' @param data data.frame
#' 
replace_na_with_lag_df <- function(data) {
  
  num_col <- sapply(data, is.numeric)
  
  from_na_to_lag <- function(x) {
    
    x_lag <- dplyr::lag(x)
    
    x[is.na(x)] <- x_lag[is.na(x)]
    
    return(x)
    
  }
  
  data[, num_col] <- sapply(data[, num_col], from_na_to_lag)
  
  return(data)
  
}