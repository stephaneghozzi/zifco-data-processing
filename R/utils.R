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
    
    time_interval_char <- paste0(time_interval, " seconds")
    
  } else if (abs(time_interval) < 3600) {
    
    time_interval_char <- paste0(round(time_interval / 60), " minutes")
    
  } else {
    
    time_interval_char <- paste0(
      round(time_interval / 3600), " hours ",
      round((time_interval %% 3600) / 60), " minutes"
    )
    
  }
  
  return(time_interval_char)
  
}