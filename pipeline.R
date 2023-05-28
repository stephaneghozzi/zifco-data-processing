t1 <- Sys.time()

# Functions and configuration ----

library(tibble)
library(tidyr)
library(dplyr)
library(DT)
load_functions <- sapply(
  list.files("R", full.names = TRUE), 
  \(x) source(x)
)

config_file <- "config.yml"
config <- yaml::read_yaml(config_file)

# Data processing ----

# Load raw data sets and meta-data
# N.B. it takes about 36 min to load the original files (27 for 
# answers.csv, 9 for backup.csv).
dataset_list_raw <- load_raw_data(config)
t2 <- Sys.time()

# PIA code book: 
# - keep initial import for printing in report
# - select relevant variables and transform the imported list in data frame
# - add variable the question ID as it appears in the data set `answers`
pia_codebook_initial <- dataset_list_raw$pia_codebook
dataset_list_raw$pia_codebook <- arrange_codebook_to_table(
  dataset_list_raw$pia_codebook
)
dataset_list_raw$pia_codebook <- add_question_id(dataset_list_raw$pia_codebook)
t3 <- Sys.time()

# Variable names
variables_dictionary <- create_variable_dictionary(config)
var_prop_std <- get_variable_properties(
  dataset_list_raw, variables_dictionary, config
)
t4 <- Sys.time()

# Standardize variable names, values, formats; clean variable values
dataset_list_std <- standardize_variable_names(dataset_list_raw, var_prop_std)
dataset_list_clean <- clean_datasets(
  dataset_list_std, var_prop_std, config$convert_time_from_germany
)
t5 <- Sys.time()

# Data set operations (filter, merge)
dataset_list <- dataset_operations(dataset_list_clean, config)
t6 <- Sys.time()

save_processed_datasets(dataset_list, config)
t7 <- Sys.time()

# Transform data sets and generate meta-data for NAKO 
for_nako <- transform_for_nako(dataset_list, config)
t8 <- Sys.time()

# Checks ----

variable_names_count <- count_variable_names(dataset_list_raw)
variable_names_checks <- check_variable_names(variable_names_count, 
  variables_dictionary)

ambiguousness_matching_clean <- check_unambiguousness_matching_ids(
  dataset_list_clean, config
)
ambiguousness_matching <- check_unambiguousness_matching_ids(dataset_list)
t9 <- Sys.time()

set.seed(34235)
sample_indices <- sample(
  1:nrow(dataset_list_clean$answers),
  round(nrow(dataset_list_clean$answers)/100),
  replace = FALSE
)
answers_sample <- dataset_list_clean$answers[sample_indices,]
pia_empty_answers_checks_sample <- check_pia_empty_answers(answers_sample)
pia_multiple_answers_checks_sample <- check_pia_multiple_answers(answers_sample)
t10 <- Sys.time()

# Save workspace ----

if (config$pipeline_ouput$save) {
  save.image(file = config$pipeline_ouput$file)  
}
t11 <- Sys.time()

# Export for NAKO ----

# Un-comment code line below to export NAKO data-sets in properly formatted 
# CSV files. 
export_for_nako(for_nako, config)
t12 <- Sys.time()

