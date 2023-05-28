# ...
clean_datasets <- function(dataset_list, var_prop_std, time_from_ger) {

  cleaned_datasets <- purrr::map2(
    names(dataset_list),
    dataset_list,
    \(nm, ds) {
      config_types <- var_prop_std |>
        dplyr::filter(dataset == nm) |>
        dplyr::select(standard, type, file_type)
      clean_variables(nm, ds, config_types, time_from_ger)
    }
  )
  names(cleaned_datasets) <- names(dataset_list)

  return(cleaned_datasets)

}

# ...
clean_variables <- function(dataset_name, dataset_df, config_types,
  time_from_ger) {

  # Format values
  cleaned_dataset <- format_specific_variable_values(dataset_name, dataset_df)

  # Convert types
  cleaned_dataset <- apply_type_conversion(cleaned_dataset, config_types,
    time_from_ger)

  return(cleaned_dataset)

}

# ...
apply_type_conversion <- function(dataset_df, config_types, time_from_ger) {

  cleaned_dataset <- dataset_df |>
    dplyr::mutate(
      dplyr::across(
        .fns = ~ convert_type(
          .x,
          config_types$type[config_types$standard == dplyr::cur_column()],
          unique(config_types$file_type),
          time_from_ger
        )
      )
    )

  return(cleaned_dataset)

}
