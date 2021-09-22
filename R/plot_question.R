#' Map question
#' @description This is a helper function for plot_question. 
#' It maps the possible choices of input$select_question to its abbreviated column names returned
#' from `construct_data`
#' @return a string with column name
#' @noRd
#' 
map_question <- function(x) {
  
  switch(x, 
         "Negativ metakognitive overbevisninger" = "neg_mind", 
         "Positiv metakognitive overbevisninger" = "pos_mind", 
         "Gamle strategier"                      = "old_strategy", 
         "Nye strategier"                        = "new_strategy")
}

#' Plot question
#' @param data tibble returned from construct_data(p = 17)
#' @param str_question string specifying which question to plot 
#' ("neg_mind", "pos_mind", "old_strategy" or "new_strategy")
#' @export
#' @return plotly object
#' @examples data <- construct_data(p = 17)
#' plot_question(data = data, str_question = "neg_mind")
#' 
plot_question <- function(data = construct_data(p = 17), str_question = "neg_mind") {
  
  str_question <- map_question(x = str_question)
  
  data %>% 
    mutate(Deltager = as.factor(Deltager)) %>% 
    dplyr::group_by(Deltager, Uge) %>% 
    summarise(neg_mind = (Spørgsmål_2 + Spørgsmål_3 + Spørgsmål_4 + Spørgsmål_5) / 4, 
              pos_mind = (Spørgsmål_6 + Spørgsmål_7 + Spørgsmål_8 + Spørgsmål_9 + Spørgsmål_10) / 5, 
              old_strategy = (Spørgsmål_11 + Spørgsmål_12 + Spørgsmål_13) / 3, 
              new_strategy = (Spørgsmål_14 + Spørgsmål_15 + Spørgsmål_16 + Spørgsmål_17) / 4) %>% 
    plotly::plot_ly(
      x = ~Uge, 
      y = ~get(str_question), 
      type = "scatter", 
      mode = "lines", 
      color = ~Deltager) %>% 
    plotly::layout(yaxis = list(title = str_question))
  
}


