# Read the PIA questionnaires and answers exported for NAKO.
# Export a samples of it as CSVs in the format indicated by NAKO.

# Choose whether to read the data from the R variables stored in an RData file
# or from the actual export for NAKO as CSV.
# WARNING:
# - importing the RData can take a few minutes
# - importing the CSV will likely take many hours.
source_file_type <- "rdata" # value can be either "rdata" or "csv"

path_to_source_file <- switch(
  source_file_type,
  rdata = "data/pipeline-output.RData",
  csv = "data/for-nako/NAKO-664/data/PIA_questionnaires"
)

# Read the NAKO export
if (source_file_type == "rdata") {
  
  tmp_env <- new.env()
  load(path_to_source_file, envir = tmp_env)
  pia_answers <- lapply(
    tmp_env$for_nako$questionnaire_names, 
    \(qn) tmp_env$for_nako$data[[qn]]
  )
  names(pia_answers) <- tmp_env$for_nako$questionnaire_names
  rm(tmp_env)
  
} else if (source_file_type == "csv") {
  
  answers_csvs <- list.files(path_to_source_file, pattern = "\\.csv$",
    full.names = TRUE, recursive = TRUE)
  pia_answers <- purrr::map(
    answers_csvs,
    read.csv,
    sep = ";",
    na = "null",
    dec = ".",
    fileEncoding = "UTF-8"
  )
  names(pia_answers) <- gsub("\\.csv", "", basename(answers_csvs))
  
} else {
  
  stop("Unknown file type ", source_file_type)
  
}

# Take samples of the NAKO data.
number_of_samples <- 10
size_of_samples <- 1000

# Export each sample as a CSV in the folder indicated.
# The path should not end with "/".
path_to_export_folder <- "data/sample-nako-export"
dir.create(path_to_export_folder, showWarnings = FALSE, recursive = TRUE)

# Sample each questionnaire
for (qn in names(pia_answers)) {
  
  if (nrow(pia_answers[[qn]]) <= size_of_samples) {
    
    pia_answers_samples <- list(pia_answers[[qn]])
    
  } else {
   
    pia_answers_samples <- lapply(
      1:number_of_samples,
      \(i) {
        set.seed(9283 + i)
        sample_indices <- sample(1:nrow(pia_answers[[qn]]), size_of_samples)
        return(pia_answers[[qn]][sample_indices,])
      }
    ) 
    
  }
  
  for (i in 1:length(pia_answers_samples)) {
    
    path_to_export_file <- paste0(
      path_to_export_folder, 
      "/", 
      qn, 
      ifelse(length(pia_answers_samples) > 1, paste0("-sample-", i), ""), 
      ".csv"
    )
    
    write.table(
      pia_answers_samples[[i]],
      file = path_to_export_file,
      quote = FALSE,
      sep = ";",
      eol = "\n",
      na = "null",
      dec = ".",
      row.names = FALSE,
      col.names = TRUE,
      qmethod = "double",
      fileEncoding = "UTF-8"
    )
    
  }
  
}