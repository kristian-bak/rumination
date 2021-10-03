#' Map question
#' @description This is a helper function for plot_question. 
#' It maps the possible choices of input$select_question to its abbreviated column names returned
#' from `construct_data`
#' @param x a string(inteded to come from input$select_question)
#' @return a string with column name
#' @noRd
#' 
map_question <- function(x) {
  
  switch(x, 
         "Spørgsmål 1"                           = "question_1",
         "Negativ metakognitive overbevisninger" = "neg_mind", 
         "Positiv metakognitive overbevisninger" = "pos_mind", 
         "Gamle strategier"                      = "old_strategy", 
         "Nye strategier"                        = "new_strategy")
}

#' Get plot data
#' @description This is a helper function for plot_question
#' It takes user specified data and returns summarised output
#' @return data with summarised questions grouped based on var_names
#' 
get_plot_data <- function(data, var_names) {
  
  data <- data %>% 
    dplyr::mutate(
      neg_mind = (Spørgsmål_2 + Spørgsmål_3 + Spørgsmål_4 + Spørgsmål_5) / 4, 
      pos_mind = (Spørgsmål_6 + Spørgsmål_7 + Spørgsmål_8 + Spørgsmål_9 + Spørgsmål_10) / 5, 
      old_strategy = (Spørgsmål_11 + Spørgsmål_12 + Spørgsmål_13) / 3, 
      new_strategy = (Spørgsmål_14 + Spørgsmål_15 + Spørgsmål_16 + Spørgsmål_17) / 4)
  
  out <- data %>% 
    dplyr::group_by(dplyr::across(var_names)) %>% 
    dplyr::summarise(
      question_1 = mean(Spørgsmål_1),
      neg_mind = mean(neg_mind), 
      pos_mind = mean(pos_mind), 
      old_strategy = mean(old_strategy), 
      new_strategy = mean(new_strategy))
  
  if ("Deltager" %notin% var_names) {
    out <- out %>% 
      dplyr::mutate(Deltager = "Gennemsnit")
  }
  
  return(out)
  
}

#' Plot question
#' @param data tibble returned from construct_data(p = 17)
#' @param str_question string specifying which question to plot 
#' ("neg_mind", "pos_mind", "old_strategy" or "new_strategy")
#' @param str_patient string specifying which patient (= Deltager) to extract
#' "1", "2", ..., "10", "Gennemsnit" or "Alle")
#' @export
#' @return plotly object
#' @examples data <- construct_data(p = 17)
#' plot_question(data = data, str_question = "neg_mind")
#' 
plot_question <- function(data = construct_data(p = 17), 
                          str_question = "neg_mind", 
                          str_patient = "1") {
  
  y_var <- map_question(x = str_question)
  
  vec_patients <- 1:10 %>% as.character()
  
  if (str_patient %in% c("Alle", "Gennemsnit")) { ## If "Alle" or "Gennemsnit" is desired
                                                  ## all data is needed to plot questions    
    input_plot_data <- data
    
    if (str_patient == "Gennemsnit") {
      var_names <- "Uge"                ## If selected patient is "Gennemsnit" then question
    } else {                            ## Deltager should be averaged out, i.e. only group by Uge
      var_names <- c("Deltager", "Uge") ## If selected patient is "Alle", then question will
    }                                   ## be summarised grouped by Deltager and Uge
    
  } else if (str_patient %in% vec_patients) {
    
    input_plot_data <- data %>%              ## If selected patient is a specific patient
      dplyr::filter(Deltager == str_patient) ## then only Uge is needed to group by
    var_names <- "Uge"
    
  }
  
  output_plot_data <- get_plot_data(data = input_plot_data, var_names = var_names)
  
  output_plot_data %>% 
    plotly::plot_ly(
      x = ~Uge, 
      y = ~get(y_var), 
      type = "scatter", 
      mode = "lines", 
      color = ~Deltager) %>% 
    plotly::layout(yaxis = list(title = str_question))
  
}


