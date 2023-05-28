# ...
count_variable_names <- function(dataset_list) {

  variable_names_count <- purrr::map2(
    names(dataset_list),
    dataset_list, 
    \(nm, ds) names(ds) |> 
      tibble::as_tibble_col(column_name = "variable_name") |> 
      dplyr::count(variable_name, name = "intra_count") |> 
      dplyr::mutate(dataset = nm)
  ) |>
    dplyr::bind_rows() |>
    dplyr::group_by(variable_name) |>
    dplyr::mutate(overall_count = sum(intra_count)) |>
    dplyr::ungroup()
  
  return(variable_names_count)
  
}

# ...
create_variable_dictionary <- function(config) {
  
  variables_dictionary <- purrr::map2(
    names(config$variables),
    config$variables,
    \(nm, ds) do.call(tidyr::expand_grid, ds) |> dplyr::mutate(standard = nm)
  ) |> 
    dplyr::bind_rows()
  
  # If `type`, `unit` or `dataset` have been completely ignored, add them as
  # empty columns.
  for (property in c("type", "unit", "dataset")) {
    if (!property %in% names(variables_dictionary)) {
      variables_dictionary[[property]] <- as.character(NA) 
    } 
  }
  
  return(variables_dictionary)
  
}

# ...
get_variable_properties <- function(dataset_list, variables_dictionary, 
  config) {
  
  var_prop_std <- purrr::map2(
    names(dataset_list),
    dataset_list,
    \(nm, ds) get_variable_properties_1(nm, ds, variables_dictionary, config)
  ) |> 
    dplyr::bind_rows()
  
  return(var_prop_std)
  
}

# ...
get_variable_properties_1 <- function(dataset_name, dataset_df, 
  variables_dictionary, config) {
  
  dict_reduced <- variables_dictionary |> 
    dplyr::filter(dataset == dataset_name | is.na(dataset))
  
  var_names <- names(dataset_df)
  empty_initial_values <- rep(as.character(NA), length(var_names))
  var_prop_std <- tibble::tibble(
    standard = empty_initial_values,
    type = empty_initial_values,
    unit = empty_initial_values,
    source = empty_initial_values
  )

  # Explicit
  if (nrow(dict_reduced) > 0 & any(var_names %in% dict_reduced$original)) {
    
    for (vn in unique(var_names)) {
    
      vn_originals <- var_names[var_names == vn]
      vn_standards <- dict_reduced |> 
        dplyr::filter(original == vn) |> 
        dplyr::select(standard, type, unit) |> 
        dplyr::mutate(source = "configuration")
      
      if (nrow(vn_standards) > 0) {
        
        if (length(vn_originals) < nrow(vn_standards)) {
          
          vn_standards <- vn_standards[1:length(vn_originals),]
          
        } else if (length(vn_originals) > nrow(vn_standards)) {
          # For the missing matches: Repeat the last standard name and apply
          # default standardization making sure not to reproduce the last 
          # standard name.
          
          vn_standards_supp <- tibble::tibble(
            standard = default_names_std(
              rep(
                tail(vn_standards$standard, 1),
                length(vn_originals) - length(vn_standards) + 1
              )
            ),
            type = tail(vn_standards$type, 1),
            unit = tail(vn_standards$unit, 1)
          )
                    
          vn_standards <- dplyr::bind_rows(
            vn_standards,
            vn_standards_supp[2:nrow(vn_standards_supp),]
          )
          
        }
        
        var_prop_std[which(var_names == vn),] <- vn_standards
        
      }
      
    }
    
  }
  
  # Default
  # Make sure not to produce a name that's already there.
  if (any(is.na(var_prop_std$standard))) {
    
    var_prop_std_def <- tibble::tibble(
      standard = default_names_std(var_names),
      type = as.character(NA),
      unit = as.character(NA),
      source = "default"
    )

    var_prop_std[is.na(var_prop_std$standard),] <- 
      var_prop_std_def[is.na(var_prop_std$standard),]  
    
  }
  
  # Last round to make sure variable names really are unique.
  var_prop_std <- var_prop_std |>
    dplyr::group_by(standard) |> 
    dplyr::mutate(position = 1:length(standard)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      suffix = dplyr::if_else(
        position == 1, 
        "", 
        paste0("_", as.character(position))
      ),
      standard = paste0(standard, suffix)
    ) |> 
    dplyr::select(-position, -suffix)
  
  # Add reference properties
  var_prop_std$original <- var_names
  var_prop_std$dataset <- dataset_name
  var_prop_std$file_type <- tail(
    stringr::str_split_1(
      config$datasets[[dataset_name]]$raw_data_file,
      "\\."
    ),
    1
  )
  
  return(var_prop_std)
  
}

# ...
default_names_std <- function(var_names) {
  
  janitor::make_clean_names(var_names)
  
}

# ...
standardize_variable_names <- function(dataset_list, var_prop_std) {
  
  dataset_list_std <- purrr::map2(
    names(dataset_list),
    dataset_list,
    \(nm, ds) {
      std_names <- var_prop_std$standard[var_prop_std$dataset == nm]
      ds |> stats::setNames(std_names)
    }
  )
  names(dataset_list_std) <- names(dataset_list)
    
  return(dataset_list_std)
  
}
