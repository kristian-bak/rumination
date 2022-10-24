#' Save plot
#' @param x plot object
#' @param file_path character string with file path
save_plot <- function(x, file_path) {
  
  png(filename = file_path)
  print(x)
  dev.off()
  
}