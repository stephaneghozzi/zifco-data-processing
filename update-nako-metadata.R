# Re-run the end of the processing pipeline, e.g., to only change the 
# description of variables and data sets the NAKO metadata exported

# Load functions and configuration
load_functions <- sapply(
  list.files("R", full.names = TRUE), 
  \(x) source(x)
)
config_file <- "config.yml"
config <- yaml::read_yaml(config_file)

# Run end of pipeline
for_nako <- transform_for_nako(dataset_list, config)
save_processed_datasets(dataset_list, for_nako, config)
export_for_nako(for_nako, config)
if (config$nako$generate_nako_samples) {
  source("sample_nako_export.R")
}
