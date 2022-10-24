#' Get plot file name
#' @param patient character string with patient ID
#' @param question character string with question (see get_questions())
#' @param filetype character string with filetype (default is png)
get_plot_file_name <- function(patient, question, filetype = "png") {
  
  paste0("Patient ", patient, " - ", question, ".", filetype)
  
}