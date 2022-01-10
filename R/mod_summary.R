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
                                choices = c("Mængden af grublerier/bekymringer", 
                                            "Negativ metakognitive overbevisninger", 
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
      ), 
      column(3, 
             shinyWidgets::awesomeCheckboxGroup(
               inputId = ns("switch_na_to_zero"), 
               label = "Erstat blanke celler med 0", 
               choices = c("Ja"), 
               selected = "Ja", 
               inline = TRUE, 
               status = "primary" 
             ), 
             style = "display:inline-block; float:left"
      )
    ),
    fluidRow(
      column(10, 
             plotly::plotlyOutput(outputId = ns("plot_summary"))
             )
      ),
    fluidRow(
      column(2, 
             downloadButton(
               outputId = ns("download_data"), 
               label = "Download data")
             ),
      column(2, 
             downloadButton(
               outputId = ns("download_template"), 
               label = "Download skabelon")
             )
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
        
        react_var$input_data_tmp <- readxl::read_excel(path = file_input()$datapath)
        
      } else {
        
        react_var$input_data_tmp <- construct_data()
        
      }
      
      if (!is.null(input$switch_na_to_zero)) {
        
        react_var$input_data <- convert_na_to_zero(data = react_var$input_data_tmp)
        
      } else {
        
        react_var$input_data <- react_var$input_data_tmp
        
      }
      
      var_questions <- paste0("Spørgsmål_", 1:17)
      
      react_var$data_to_download <- react_var$input_data %>% 
        dplyr::select(c("Deltager", "Uge", var_questions))
      
    })
    
    output$plot_summary <- plotly::renderPlotly({
      
      plot_question(
        data = react_var$input_data, 
        str_question = input$select_question, 
        str_patient = input$select_patient
      )

    })
    
    output$download_data <- download_excel(
      data = react_var$data_to_download, 
      file_name = "Rumination.xlsx"
    )
    
    output$download_template <- download_excel(
      data = create_template(data = react_var$data_to_download), 
      file_name = "Rumination.xlsx"
    )
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
