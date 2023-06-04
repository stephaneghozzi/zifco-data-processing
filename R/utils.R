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

print_computation_time <- function(t1, t2) {
  # Print duration between POSIX dates `t1` and `t2` in seconds, minutes and/or
  # minutes
   
  time_interval <- as.numeric(ceiling(difftime(t1, t2, units = "secs")))
  
  if (abs(time_interval) < 60) {
    
    time_interval_char <- sinplu(time_interval, "second")
    
  } else if (abs(time_interval) < 3600) {
    
    time_interval_char <- sinplu(round(time_interval / 60), "minute")
    
  } else {
    
    time_interval_char <- paste0(
      sinplu(floor(time_interval / 3600), "hour"),
      " ",
      sinplu(round((time_interval %% 3600) / 60), "minute")
    )
    
  }
  
  return(time_interval_char)
  
}

sinplu <- function(quantity, noun) {
  
  paste0(quantity, " ", noun, ifelse(abs(quantity) > 1, "s", ""))
  
}
