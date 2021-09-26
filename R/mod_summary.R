#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fluidPage column selectInput
mod_summary_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4, 
             shiny::selectInput(inputId = ns("select_question"), 
                                label = "Spørgsmål", 
                                choices = c("Negativ metakognitive overbevisninger", 
                                            "Positiv metakognitive overbevisninger", 
                                            "Gamle strategier", 
                                            "Nye strategier")
             )
      ), 
      column(2, 
             shiny::selectInput(inputId = ns("select_patient"), 
                                label = "Deltager",
                                choices = c(1:10, "Gennemsnit", "Alle")
             )
      )
    ),
    fluidRow(
      column(10, 
             plotly::plotlyOutput(outputId = ns("plot_summary"))
             )
      ),
    fluidRow(
      downloadButton(outputId = ns("download_data"), label = "Download data")
    )
  )
}
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, file_input) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    react_var <- reactiveValues()
    
    observe({
      
      if (!is.null(file_input())) {
        
        react_var$input_data <- readxl::read_excel(path = file_input()$datapath)
        
      } else {
        
        react_var$input_data <- construct_data(p = 17)
        
      }
      
    })
    
    output$plot_summary <- plotly::renderPlotly({
      
      plot_question(
        data = react_var$input_data, 
        str_question = input$select_question, 
        str_patient = input$select_patient
      )

    })
    
    output$download_data <- download_excel(
      data = react_var$input_data, 
      file_name = "Rumination.xlsx"
    )
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
