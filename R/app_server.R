#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  
  file_input <- mod_upload_server("upload_ui_1")
  
  mod_summary_server(id = "summary_ui_1", file_input = file_input)
  
}
