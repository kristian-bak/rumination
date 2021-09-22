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
      column(3, 
             shiny::selectInput(inputId = ns("select_question"), 
                                label = "Question", 
                                choices = c("Negativ metakognitive overbevisninger", 
                                            "Positiv metakognitive overbevisninger", 
                                            "Gamle strategier", 
                                            "Nye strategier")
             )
      ), 
      column(2, 
             shiny::selectInput(inputId = ns("select_participant"), 
                                label = "Deltager",
                                choices = c(1:10, "Gennemsnit")
             )
      )
    ),
    fluidRow(
      column(10, 
             plotly::plotlyOutput(outputId = ns("plot_summary"))
             )
      )
  )
}
    
#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    data <- construct_data(p = 17)
    
    output$plot_summary <- plotly::renderPlotly({
      plot_question(data = data, str_question = input$select_question)
    })
 
  })
}
    
## To be copied in the UI
# mod_summary_ui("summary_ui_1")
    
## To be copied in the server
# mod_summary_server("summary_ui_1")
