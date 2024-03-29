library(shinydashboard)

#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  # Leave this function for adding external resources
  golem_add_external_resources()
  
  shinydashboard::dashboardPage(
    dashboardHeader(title = "Rumination"),
    dashboardSidebar(
      
      show_version_number(),
      
      sidebarMenu(
        menuItem("Udvikling", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Upload data", tabName = "Upload", icon = icon("th"))
      )
      
    ),
    
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                
                mod_summary_ui("summary_ui_1")
                
        ),
        
        # Second tab content
        tabItem(tabName = "Upload",
                mod_upload_ui("upload_ui_1")
        )
      )
    )
    
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'rumination'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

