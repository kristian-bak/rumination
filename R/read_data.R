#' Read data
#' @param path path to the xlsx file to load (default is "./data/Rumination.xlsx")
#' @details Missing path occurs locally in development phase 
#' and NULL path occurs in the app when no file has been uploaded
read_data <- function(path) {
  
  if (missing(path)) {
    
    path <- "./data/Rumination.xlsx"
    
  }
  
  if (is.null(path)) {
    
    path <- "./data/Rumination.xlsx"
    
  }
  
  readxl::read_excel(path = path)
  
}