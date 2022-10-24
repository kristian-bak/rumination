#' Get file name from patient and question
get_file_name_from_patient_and_question <- function(str_patient, str_question) {
  
  str_question[str_question == "Mængden af grublerier/bekymringer"] <- "bekymringer"
  str_question[str_question == "Negativ metakognitive overbevisninger"] <- "negativ_overbevisning"
  str_question[str_question == "Positiv metakognitive overbevisninger"] <- "positiv_overbevisning"
  str_question[str_question == "Gamle strategier"] <- "gamle_strategier"
  str_question[str_question == "Nye strategier"] <- "nye_strategier"
  
  out <- paste0("patient", str_patient, "_", str_question)
  
  return(out)
  
}

#' Get plot as list
#' @param tibble tibble with patients
#' 
get_plots_as_list <- function(data) {
  
  questions_org <- get_questions()
  questions_mod <- questions_org %>% gsub(pattern = "/", replacement = "-", x = .)
  patients      <- get_patients(data)
  
  n_questions <- length(questions_org)
  n_patients  <- length(patients)
  
  plot_list <- list()
  
  df <- tidyr::expand_grid(str_patient = patients, str_question = questions_org, plotly = FALSE) %>% 
    dplyr::mutate(file_name = get_file_name_from_patient_and_question(str_patient, str_question), 
                  data = list(data))
  
  file_name <- df$file_name
  
  df <- df %>% 
    dplyr::select(-file_name) %>% 
    dplyr::relocate(data)
  
  plot_list <- purrr::pmap(.l = df, .f = plot_question)
  
  names(plot_list) <- file_name
  
  return(plot_list)
  
}
