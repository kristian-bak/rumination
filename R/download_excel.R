#' Download excel
#' @param data data to download
#' @param file_name file name including extension. Should be .xlsx
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
        row.names = FALSE
      )
    }
  )
}
