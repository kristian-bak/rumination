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
             shiny::radioButtons(
               inputId = ns("switch_na"), 
               label = "Datahåndtering", 
               choices = list("Ingen databehandling" = 0, 
                              "Erstat blanke celler med forrige værdi" = 1,
                              "Erstat blanke celler med regressionslinje" = 2), 
               selected = 0
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
    
    ## This ensures that select input is not reset when ticking input box
    observe({
      react_var$previous_selected_patient <- input$select_patient
    })
    
    react_input_data <- reactive({
      
      if (is.null(file_input())) {
        return(data.frame())
      }
      
      readxl::read_excel(path = file_input()$datapath)
      
    })
    
    react_plot_data <- reactive({
      react_input_data() %>% 
        prep_cols(option = input$switch_na)
    })
    
    observe({
      
      if (!is.null(file_input())) {
        
        all_patients <- c(react_input_data() %>% dplyr::pull(Deltager) %>% unique(), 
                          "Gennemsnit", "Alle")
        
        updateSelectInput(
          session = session, 
          inputId = "select_patient", 
          choices = all_patients, 
          selected = react_var$previous_selected_patient
        )
        
      } else {
        
        react_var$input_data_tmp <- construct_data()
        
      }
      
    })
    
    output$plot_summary <- plotly::renderPlotly({
      
      if (nrow(react_plot_data()) == 0) {
        return()
      }
      
      plot_question(
        data = react_plot_data(), 
        str_question = input$select_question, 
        str_patient = input$select_patient
      )

    })
    
    react_data_to_download <- reactive({
      
      var_questions <- paste0("Spørgsmål_", 1:17)
      
      react_plot_data() %>% 
        dplyr::select(c("Deltager", "Uge", var_questions))
    })
    
    output$download_data <- download_excel(
      data = react_data_to_download(), 
      file_name = "Rumination.xlsx"
    )
    
    output$download_template <- download_excel(
      data = create_template(data = react_data_to_download()), 
      file_name = "Rumination.xlsx"
    )
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
