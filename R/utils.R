percentage <- function(x, n) {
  
  round((10^(n+2)) * x) / 10^n
  
}

null_to_na <- function(x) {
  
  ifelse(is.null(x), NA, x)
  
}

replace_null_default <- function(test_name, default_name) {
  
  ifelse(
    is.null(test_name), 
    default_name, 
    test_name
  )
  
}

