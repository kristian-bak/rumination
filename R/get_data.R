#' Integer rnorm
#' @param n number of observations passed to rnorm
#' @param mean mean passed to rnorm
#' @param sd standard deviation passed to rnorm
integer_rnorm <- function(n, mean, sd) {
  
  z <- rnorm(n = n, mean = mean, sd = 1) %>% round(0)
  
  y <- pmax(z, 0)
  y <- pmin(y, 10)
  
  return(y)
  
}

#' Get random vector
#' @param data_pmap a tibble with parameters to pmap through rnorm
#' 
get_random_vector <- function(data_pmap) {
  
  purrr::pmap(.l = data_pmap, .f = integer_rnorm) %>% 
    unlist()
  
}

#' Validate data
#' @param data tibble with Uge column
#' 
validate_data <- function(data) {
  if ("Uge" %notin% names(data)) {
    stop("Column named 'Uge' should be present in data")
  }
}

#' Simulate linear decrease
#' @param data_pmap data.frame with arguments for rnorm to pmap through
simulate_linear_decrease <- function(data) {
  
  data_pmap <- data %>% 
    dplyr::mutate(n = 1, mean = rev(Uge), sd = 1) %>% 
    dplyr::select(n, mean, sd)
  
  get_random_vector(data_pmap = data_pmap)
  
}

#' Simulate zero change
#' @param data_pmap data.frame with arguments for rnorm to pmap through
simulate_zero_change <- function(data) {
  
  data_pmap <- data %>% 
    dplyr::mutate(n = 1, mean = 5, sd = 0.5) %>% 
    dplyr::select(n, mean, sd)
  
  get_random_vector(data_pmap = data_pmap)
  
}

#' Simulate concave decrease
#' @param data_pmap data.frame with arguments for rnorm to pmap through
simulate_concave_decrease <- function(data) {
  
  data_pmap <- data %>% 
    dplyr::mutate(n = 1, mean = rev(Uge) %>% log() + 6, sd = 1) %>% 
    dplyr::select(n, mean, sd)
  
  get_random_vector(data_pmap = data_pmap)
  
}

#' Simulate convex decrease
#' @param data_pmap data.frame with arguments for rnorm to pmap through
simulate_convex_decrease <- function(data) {
  
  data_pmap <- data %>% 
    dplyr::mutate(n = 1, mean = Uge, sd = 1) %>%
    dplyr::mutate(mean = 50 / (5 + log(mean))) %>% 
    dplyr::select(n, mean, sd)
  
  get_random_vector(data_pmap = data_pmap)
  
}

simulate_data <- function(data, trend) {
  
  if (trend %notin% c("linear decrease", "zero change", "convex decrease", "concave decrease")) {
    stop("Possible values for trend input are: 'linear decrease', 'zero change', 'convex decrease' and 'concave decrease'")
  }
  
  validate_data(data = data)
  
  if (trend == "linear decrease") {
    
    simulate_linear_decrease(data = data)
    
  } else if (trend == "zero change") {
    
    simulate_zero_change(data = data)
    
  } else if (trend == "convex decrease") {
    
    simulate_convex_decrease(data = data)
    
  } else if (trend == "concave decrease") {
    
    simulate_concave_decrease(data = data)
    
  }
  
}

#' Get random data
#' @param data tibble with Uge column
#' @param trend string specifying which simulate_data function to use('linear decrease', 'unchanged', 'convex decrease' or 'concave decrease')
#' @param p number of predictors
#' @export
get_random_data <- function(data, trend, p) {
  n <- nrow(data)
  mat <- matrix(NA, nrow = n, ncol = p)
  for (i in 1:p) {
    mat[, i] <- simulate_data(data = data, trend = trend[i])
  }
  
  colnames(mat) <- paste0("Spørgsmål_", 1:p)
  
  out <- mat %>% 
    tibble::as_tibble()
  
  return(out)
  
}

#' Construct data
#' @return tibble with columns Deltager, Uge and simulated answers for predictors 1:17
#' @export
construct_data <- function() {
  
  data <- tibble::tibble(
    Deltager = rep(1:10, each = 8) %>% as.factor(), 
    Uge = rep(1:8, 10)
  )
  
  trend <- c(rep("linear decrease", 5),
             rep("zero change", 5), 
             rep("convex decrease", 3),
             rep("concave decrease", 4))
  
  p <- length(trend)
  
  questions <- get_random_data(
    data = data, 
    trend = trend, 
    p = p
  )
  
  data <- dplyr::bind_cols(data, questions)
  
  data <- data %>% 
    dplyr::mutate(
      neg_mind = (Spørgsmål_2 + Spørgsmål_3 + Spørgsmål_4 + Spørgsmål_5) / 4, 
      pos_mind = (Spørgsmål_6 + Spørgsmål_7 + Spørgsmål_8 + Spørgsmål_9 + Spørgsmål_10) / 5, 
      old_strategy = (Spørgsmål_11 + Spørgsmål_12 + Spørgsmål_13) / 3, 
      new_strategy = (Spørgsmål_14 + Spørgsmål_15 + Spørgsmål_16 + Spørgsmål_17) / 4)
  
  return(data)
  
}
