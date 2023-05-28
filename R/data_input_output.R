# ...
load_raw_data <- function(config) {
  
  global_dataset_config <- config$datasets$all_data_sets
  dataset_config <- config$datasets[
    names(config$datasets) != "all_data_sets"
  ]
  
  dataset <- lapply(
    dataset_config, 
    \(x) {
      x$global_dataset_config <- global_dataset_config
      do.call(raw_data_io, x)
    }
  )
  dataset[sapply(dataset, is.null)] <- NULL
  
  return(dataset)
  
}

# ...
raw_data_io <- function(ignore_on_loading = NULL, raw_data_file = NULL, 
  raw_data_native_format_file = NULL, save_native = NULL,
  read_native = NULL, ..., global_dataset_config = NULL) {
  
  skip_loading <- FALSE
  
  # Skip data sets that will be produced later during processing
  if (!is.null(ignore_on_loading)) {
    if(ignore_on_loading) {
      
      skip_loading <- TRUE
      
    }
  }
  
  if (skip_loading) {
    
    dataset <- NULL
    
  } else {
    
    # Read data
    if (!is.null(read_native)) {
      
      rfn <- read_native
      
    } else if (!is.null(global_dataset_config$read_native)) {
      
      rfn <- global_dataset_config$read_native
      
    } else {
      
      stop("I need to know whether to read from native or from raw data file. ",
        "Please set `read_native` in configuration file. ")
      
    }
    
    if (rfn) {
      
      if (is.null(raw_data_native_format_file)) {
        stop("I need to know the name of the native data file.") 
      } else {
        dataset_file <- paste0(
          ifelse(
            is.null(global_dataset_config$native_dir),
            "",
            paste0(global_dataset_config$native_dir, "/")
          ),
          raw_data_native_format_file
        )
      }
      
    } else {
      
      if (is.null(raw_data_file)) {
        stop("I need to know the name of the raw data file.") 
      } else {
        dataset_file <- paste0(
          ifelse(
            is.null(global_dataset_config$raw_dir),
            "",
            paste0(global_dataset_config$raw_dir, "/")
          ),
          raw_data_file
        )
      }
      
    }
    
    dataset <- load_dataset(dataset_file)
    
    # Save data in native format
    if (!is.null(save_native)) {
      
      stn <- save_native
      
    } else if (!is.null(global_dataset_config$save_native)) {
      
      stn <- global_dataset_config$save_native
      
    } else {
      
      stop("I need to know whether to save to native data file. ",
        "Please set `save_native` in configuration file. ")
      
    }
    
    if (stn) {
      
      raw_native_dataset_file <- paste0(
        ifelse(
          is.null(global_dataset_config$native_dir),
          "",
          paste0(global_dataset_config$native_dir, "/")
        ),
        raw_data_native_format_file
      )
      
      dir.create(
        dirname(raw_native_dataset_file), recursive = TRUE, showWarnings = FALSE
      )
      saveRDS(dataset, file = raw_native_dataset_file)
      
    }
    
  }
  
  return(dataset)
  
} 

# ...
load_dataset <- function(dataset_file) {
  
  if (dir.exists(dataset_file)) {
    files <- list.files(path = dataset_file, full.names = TRUE)
    dataset <- list()
  } else {
    files <- dataset_file
  }
  
  for (dsf in files) {
  
    dsf_split <- stringr::str_split_1(basename(dsf), "\\.")
    dsf_sub_name <- paste(dsf_split[-length(dsf_split)], collapse = ".")
    file_extension <- tolower(tail(dsf_split, 1))
    
    if (file_extension == "csv") {
      
      ds <- tibble::as_tibble(
        utils::read.csv(dsf, na.strings = c("", ".", "NA"), 
          colClasses = "character", encoding = "UTF-8", check.names = FALSE)
      )
      
    } else if (file_extension == "xlsx") {
      
      ds <- readxl::read_excel(dsf, col_types = "text",
        .name_repair = "minimal")
      
    } else if (file_extension == "rds") {      
      
      ds <- readRDS(dsf)
      
    } else if (file_extension == "json") {
      
      ds <- rjson::fromJSON(file = dsf, simplify = TRUE)
      
    } else {
      
      stop("Unrecognized file format ", file_extension, ".")
      
    }
    
    if (dir.exists(dataset_file)) {
      dataset[[dsf_sub_name]] <- ds
    } else {
      dataset <- ds
    }
    
  }
  
  return(dataset)
  
} 

# ...
save_processed_datasets <- function(dataset_list, for_nako, config) {
  
  global_out_path <- config$datasets$all_data_sets$processed_dir
  out_path_all <- ifelse(
    !is.null(global_out_path),
    paste0(global_out_path, "/"),
    ""
  )
  
  datasources <- list(general = dataset_list, nako = for_nako)
  
  for (ds in names(datasources)) {
    
    datalist <- datasources[[ds]]
  
    for (nm in names(datalist)) {
      
      if (ds == "general") {
        custom_file_name <- config$datasets[[nm]]$clean_data_native_format_file
      } else {
        custom_file_name <- NULL
      }
      
      if (ds == "nako") {
        out_path <- paste0(out_path_all, "for_nako/")
      } else {
        out_path <- out_path_all
      }
      
      full_file_name <- ifelse(
        !is.null(custom_file_name),
        paste0(out_path, custom_file_name),
        paste0(out_path, nm, ".rds")
      )
      
      dir.create(
        dirname(full_file_name), recursive = TRUE, showWarnings = FALSE
      )
      
      saveRDS(datalist[[nm]], file = full_file_name) 
      
    }
    
  }
  
}

