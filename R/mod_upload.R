#' upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS fluidRow column fileInput
mod_upload_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6, 
           fileInput(inputId = ns("browse_file"), 
                     label = "Vælg excel fil", 
                     accept = "xlsx")
           )
  )
}
    
#' upload Server Functions
#'
#' @noRd 
mod_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    react_var <- reactiveValues(data = data.frame())
    
    observe({
      
      uploaded_file <- input$browse_file
      if (is.null(uploaded_file))
        return(NULL)
      
      data <- readxl::read_excel(path = uploaded_file$datapath)
      
      var_needed <- c("Deltager", "Uge", 
                      paste0("Spørgsmål_", 1:17))
      
      is_numeric <- sapply(X = var_needed[-1], FUN = function(x) {is.numeric(data[[x]])})
      
      if (!all(is_numeric)) {
        showNotification(
          ui = "Mindst en af søjlerne: Uge, Spørgsmål_1, Spørgsmål_2, ..., Spørgsmål_17 indeholder karaktertegn. Søjlerne bør være udelukkende numeriske", 
          duration = 7, 
          type = "error"
        )
        return(NULL)
      }
      
      
      if (!all(var_needed %in% names(data))) {
        showNotification(
          ui = "Forkert excel fil. Data bør indeholder søjlerne Deltager, Uge, Spørgsmål_1, Spørgsmål_2, ..., Spørgsmål_17", 
          type = "error", 
          duration = 7
        )
        return(NULL)
      }
      
      react_var$data <- data
      
      ## Using a sleeper to prevent eyes from getting confused
      Sys.sleep(0.6)
      
      showNotification(
        ui = "Graferne er nu lavet med dine data", 
        duration = 10, 
        type = "message"
      )
      
    })
    
    out <- reactive(input$browse_file)
    
    return(out)
 
  })
}
    
## To be copied in the UI
# moload_ui("upload_ui_1")d_up
    
## To be copied in the server
# mod_upload_server("upload_ui_1")
