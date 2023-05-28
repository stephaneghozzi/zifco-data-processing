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
  rdata = "S:/PROJACTIVE/ZIFCO-NAKO-Daten/i.Vacc_Skripte/ZIFCO_cleaning/stephane-ghozzi/zifco-data-processing/data/pipeline-output.RData",
  csv = "S:/PROJACTIVE/ZIFCO-NAKO-Daten/i.Vacc_Skripte/ZIFCO_cleaning/stephane-ghozzi/zifco-data-processing/data/for-nako/NAKO-664/data/PIA_questionnaires/PIA_answers.csv"
)

# Read the NAKO export
if (source_file_type == "rdata") {

  tmp_env <- new.env()
  load(path_to_source_file, envir = tmp_env)
  pia_answers <- tmp_env$for_nako$data$answers
  rm(tmp_env)

} else if (source_file_type == "csv") {

  pia_answers <- read.csv(
    file = path_to_source_file,
    sep = ";",
    eol = "\n",
    na = "null",
    dec = ".",
    fileEncoding = "UTF-8"
  )

} else {

  stop("Unknown file type ", source_file_type)

}

# Take samples of the NAKO data.
number_of_samples <- 10
size_of_samples <- 1000

pia_answers_samples <- lapply(
  1:number_of_samples,
  \(i) {
    set.seed(9283 + i)
    sample_indices <- sample(1:nrow(pia_answers), size_of_samples)
    return(pia_answers[sample_indices,])
  }
)

# Export each sample as a CSV in the folder indicated.
# The path should not end with "/".
path_to_export_folder <- "C:/Users/sgh22/Documents/sample-nako-export"
dir.create(path_to_export_folder, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(pia_answers_samples)) {

  path_to_export_file <- paste0(
    path_to_export_folder, "/nako-export-sample-", i, ".csv"
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

