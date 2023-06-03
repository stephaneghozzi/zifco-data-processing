# ...
convert_type <- function(x, type, read_from_excel, time_from_ger) {
  
  if(is.factor(x)) {
    # Make sure we don't have factors.
    x <- as.character(x)
  }
  
  if (is.na(type)) {
    
    x_ <- x
    
  } else if (type == "boolean") {
    
    x_ <- as.logical(x)
    
  } else if (type == "character") {
    
    x_ <- as.character(x)
    
  } else if (type %in% c("date", "date_time")) {
    
    x_ <- get_date_time(x, type, read_from_excel, time_from_ger) 
    
  } else if (type == "float") {
    
    x_ <- as.double(x)
    
  } else if (type == "integer") {
    
    x_ <- as.integer(x)
    
  } else {
    
    stop("I don't know how to convert to type ", type, ".")
    
  }
  
  return(x_)   
  
}

# ...
get_date_time <- function(x, type, read_from_excel, time_from_ger) {
  # Date and date-time values are assumed to be either:
  # - for more than half of them, integers or strings containing only digits, 
  # which is the internal numeric representation of dates in R and other
  # languages
  # - for Excel, for more than half of them, numeric or strings containing
  # only digits and possibly one dot; we assume Excel for Windows, Excel 2016 
  # for Mac, or Excel for Mac 2011 was used (other reference used for earlier 
  # versions for Mac)
  # - mostly well formatted strings; then we try to guess
  # 
  # In the first two cases, the time zone is assumed to be Germany with 
  # daylight saving times as of 2022: we first read setting time zone to UTC,
  # and then have to remove one or two hours depending on daylight saving.
  # References: 
  # - https://support.microsoft.com/en-us/office/date-systems-in-excel-e7fe7167-48a9-4b96-bb53-5612a800b487
  # - https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r
  
  x_int <- is.integer(x) | grepl("^[0-9]+$", x)
  x_num_xl <- is.numeric(x) | grepl("^[0-9]+$", x) | 
    grepl("^[0-9]+\\.[0-9]+$", x)
  
  if (sum(x_int) > length(x)/2) {
    
    x <- suppressWarnings(as.numeric(x))
    x_ <- as.POSIXct(x*24*60*60, origin = "1970-01-01", tz = "UTC")
    
  } else if (read_from_excel & sum(x_num_xl) > length(x)/2) {  
    
    x <- suppressWarnings(as.numeric(x))
    x_ <- as.POSIXct(x*24*60*60, origin = "1899-12-30", tz = "UTC")
    
  } else {
    
    if (type == "date") {
      
      x_ <- lubridate::parse_date_time(x, orders = c("ymd", "dmy", "mdy"), 
        tz = "UTC")
      
    } else {
      
      x_ <- lubridate::parse_date_time(x, orders = c("ymd_HMS", "dmy_HMS", 
        "mdy_HMS"), tz = "UTC") 
      
    }
    
  }
  
  if (type == "date") {
    
    x_c <- as.Date(x_)
    
  } else if (time_from_ger) {
  
    x_c <- convert_German_timezone_to_UTC(x_)
    
  }
  
  return(x_c)
  
}

#...
convert_German_timezone_to_UTC <- function(x_) {
  # Take the date-time `x_` assuming it is actually the time in Germany (and not
  # UTC as assumed in the variable). Then remove one or two hours depending on
  # the date: as of 2022, time zone in Germany is CET = UTC+1 in winter and 
  # CEST = UTC+2 in summer.
  # Reference: https://www.timeanddate.com/time/change/germany
  
  transition_times <- c(
    CEST_to_CET = "10-30 01:00:00",
    CET_to_CEST = "03-27 01:00:00"
  )
  
  ref_times <- list()
  for (tt in names(transition_times)) {
    
    ref_times[[tt]] <- as.POSIXct(
      sapply(
        x_,
        \(dt) ifelse(
          is.na(dt),
          NA,
          paste0(lubridate::year(dt), "-", transition_times[[tt]])
        )
      ),
      tz = "UTC"
    )
    
  }
  
  x_c <- sapply(
    1:length(x_),
    \(i) ifelse(
      is.na(x_[i]),
      NA,
      ifelse(
        x_[i] >= ref_times$CET_to_CEST[i] & 
          x_[i] < ref_times$CEST_to_CET[i], 
        x_[i] - 2 * 3600,
        x_[i] - 3600
      )
    )
  ) |> 
    as.POSIXct(origin = "1970-1-1", tz = "UTC")
  
  return(x_c)
  
}

