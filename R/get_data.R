#' Get random vector
#' @param data_pmap data.frame with arguments for rnorm to pmap through
#' @export
get_random_vector <- function(data_pmap) {
  purrr::pmap(.l = data_pmap, .f = rnorm) %>% 
    unlist()
}

#' Get random data
#' @param data_pmap data.frame with arguments for rnorm to pmap through
#' @param p number of predictors
#' @export
get_random_data <- function(data_pmap, p) {
  n <- nrow(data_pmap)
  mat <- matrix(NA, nrow = n, ncol = p)
  for (i in 1:p) {
    mat[, i] <- get_random_vector(data_pmap)
  }
  
  colnames(mat) <- paste0("Spørgsmål_", 1:p)
  
  out <- mat %>% 
    tibble::as_tibble()
  
  return(out)
  
}

#' Construct data
#' @param p number of predictors. Default is 17. 
#' @return tibble with columns Deltager, Uge and simulated answers for predictors 1:p
#' @export
construct_data <- function(p = 17) {
  
  data <- tibble::tibble(Deltager = rep(1:10, each = 8) %>% as.factor(), 
                 Uge = rep(1:8, 10))
  
  data_pmap <- data %>% 
    dplyr::mutate(n = 1, mean = rev(Uge), sd = 1) %>% 
    dplyr::select(n, mean, sd)
  
  questions <- get_random_data(data_pmap = data_pmap, p = p)
  
  data <- dplyr::bind_cols(data, questions)
  
  data <- data %>% 
    dplyr::mutate(
      neg_mind = (Spørgsmål_2 + Spørgsmål_3 + Spørgsmål_4 + Spørgsmål_5) / 4, 
      pos_mind = (Spørgsmål_6 + Spørgsmål_7 + Spørgsmål_8 + Spørgsmål_9 + Spørgsmål_10) / 5, 
      old_strategy = (Spørgsmål_11 + Spørgsmål_12 + Spørgsmål_13) / 3, 
      new_strategy = (Spørgsmål_14 + Spørgsmål_15 + Spørgsmål_16 + Spørgsmål_17) / 4)
  
  return(data)
  
}