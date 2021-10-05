show_version_number <- function(package = "rumination") {
  
  version_number <- packageVersion(pkg = package) %>% as.character()
  version_number <- paste0("v", version_number)
  
  div(version_number, 
      style = "text-align:center")
  
}