#' Replace with NA
#' @param x any input
#' @description This is a helper function for `create_template`. It is used to replace cell values with NA
#' @return NA
replace_with_na <- function(x) {
  return(NA)
}

#' Create template
#' @param data data with Deltager, Uge and questions
#' 
create_template <- function(data) {
  
  var_overwrite <- paste0("Spørgsmål_", 1:17)
  
  data %>% 
    dplyr::mutate(dplyr::across(.cols = var_overwrite, .fns = replace_with_na))
  
}

#' Download excel
#' @param data data to download
#' @param file_name file name including extension. Should be .xlsx
#' This means all questions will be overwritten to NA. Default is FALSE. 
#' @return excel file
#' @export
#' 
download_excel <- function(data, file_name) {
  
  shiny::downloadHandler(
    filename = file_name,
    content = function(file) {
      xlsx::write.xlsx(
        x = as.data.frame(data), ## It's important to convert x to data.frame to prevent bug
        file = file, 
        sheetName = "data", 
        row.names = FALSE, 
        showNA = FALSE
      )
    }
  )
}
