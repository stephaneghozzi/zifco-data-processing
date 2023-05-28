transform_for_nako <- function(dataset_list, config) {
  # The requirements of the NAKO regarding data and meta-data are described
  # (in German) here: https://transfer.nako.de/transfer/media/TFS-Info-12a_Informationen_zu_Metadaten+Ergebnisdatenuebermittlung_v2_final.pdf
  
  dataset_list_nako <- dataset_list
  
  # Add PIA code book to answers
  dataset_list_nako <- add_codebook_to_answers(dataset_list_nako)
  
  # Keep only relevant data sets and variables 
  dataset_list_nako <- keep_only_relevant_for_nako(dataset_list_nako, config)
  
  # Remove entries with missing participant ID
  dataset_list_nako <- remove_missing_participants(dataset_list_nako)
  
  # Format variable names
  var_dict_nako <- create_var_dict_nako(dataset_list_nako, config)
  dataset_list_nako <- format_variable_names_for_nako(
    dataset_list_nako, var_dict_nako
  )
  
  # Format dates and date-times
  dataset_list_nako <- format_dates_times_for_nako(dataset_list_nako)
  
  # Code missing values
  dataset_list_nako <- code_missing_values_for_nako(dataset_list_nako, config)
  
  # Move column of participant pseudonyms `Proband` to first position
  dataset_list_nako <- move_participant_to_front_for_nako(dataset_list_nako)
  
  # Split answers into different questionnaires
  questionnaire_col <- var_dict_nako$nako[
    var_dict_nako$original == "questionnaire_name"
  ]
  dataset_list_nako_s <- split_answers_in_questionnaires_for_nako(
    dataset_list_nako, questionnaire_col
  )
  
  # Generate the meta-data
  questionnaire_names <- standardize_questionnaire_names(
    unique(dataset_list_nako$answers[[questionnaire_col]])
  )
  metadata_nako <- generate_meta_data_for_nako(
    dataset_list_nako_s, questionnaire_names, config, var_dict_nako
  )
  
  for_nako <- list(
    metadata = metadata_nako,
    data = dataset_list_nako_s,
    questionnaire_names = questionnaire_names
  )
  
  return(for_nako)
  
}

# ...
add_codebook_to_answers <- function(dataset_list_nako) {
  
  if (all(c("answers", "pia_codebook") %in% names(dataset_list_nako))) {
    
    dataset_list_nako$answers <- dataset_list_nako$answers |> 
      dplyr::left_join(dataset_list_nako$pia_codebook, by = "question_single")
    
  }
  
  return(dataset_list_nako)
  
}

# ...
keep_only_relevant_for_nako <- function(dataset_list_nako, config) {
  
  # Keep only data sets relevant for NAKO
  dataset_list_nako <- dataset_list_nako[config$nako$keep_datasets]
  
  # Remove variables not relevant for NAKO
  dataset_list_nako <- lapply(
    dataset_list_nako, 
    \(ds) ds |> 
      dplyr::select(
        -dplyr::any_of(config$nako$remove_vars)
      )
  )
  
  return(dataset_list_nako)
  
}

# ...
remove_missing_participants <- function(dataset_list_nako) {
  
  for (nm in names(dataset_list_nako)) {
    
    dataset_list_nako[[nm]] <- dataset_list_nako[[nm]] |> 
      dplyr::filter(!is.na(participant_id))
    
  }
  
  return(dataset_list_nako)
  
}

# ...
create_var_dict_nako <- function(dataset_list_nako, config) {
  # - add prefix "u" and application (Antrag) number at the beginning, e.g., 
  # for application 664, add "u664_" at the beginning
  # - exception is participant id which is always called "Proband"
  # - only ASCII letters, numbers, underscore
  # - max 20 symbols
  
  var_names_original <- sapply(dataset_list_nako, \(ds) names(ds)) |> 
    unlist() |> 
    stats::setNames(NULL) |> 
    unique()
  
  pos_part <- which(var_names_original == "participant_id")
  
  var_names_mod <- paste0("u", config$nako$application_number, "_", 
    gsub("[^A-z0-9]", "_", tolower(var_names_original)))
  
  var_dict_nako <- tibble::tibble(
    var_names = var_names_mod
  ) |> 
    dplyr::mutate(
      var_names_short = substr(var_names, 1, 18),
      var_names_shorter = substr(var_names, 1, 16)
    ) |> 
    dplyr::group_by(var_names_shorter) |> 
    dplyr::mutate(
      div = length(unique(var_names)),
      ran = rank(unique(var_names))
    ) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      nako = ifelse(
        div > 1, 
        paste0(var_names_shorter, "_", ran), 
        var_names_short
      )
    ) |> 
    dplyr::select(nako)
  
  if (any(nchar(var_dict_nako$nako) > 18)) {
    stop(
      "Some variable names are too long, please shorten: ", 
      paste(
        var_names_original[nchar(var_dict_nako$nako) > 18], 
        collapse = ", "
      ), 
      "."
    )
  }
  
  if (length(pos_part) > 0) { 
    var_dict_nako$nako[pos_part] <- "Proband"
  }
  
  var_dict_nako$original <- var_names_original
  
  return(var_dict_nako)
  
}

# ...
format_variable_names_for_nako <- function(dataset_list_nako, var_dict_nako) {
  
  for (nm in names(dataset_list_nako)) {
    
    names(dataset_list_nako[[nm]]) <- sapply(
      names(dataset_list_nako[[nm]]), 
      \(vn) var_dict_nako$nako[var_dict_nako$original == vn]
    ) |> stats::setNames(NULL)
    
  }
  
  return(dataset_list_nako)
  
}

# ...
format_dates_times_for_nako <- function(dataset_list_nako) {
  # date: "yyyy-MM-dd"
  # date-time: "yyyy-MM-dd HH-mm-ss" 24h
  
  for (nm in names(dataset_list_nako)) {
    
    dataset_list_nako[[nm]] <- dataset_list_nako[[nm]] |> 
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(\(x) inherits(x, "Date")),
          \(x) format(x, "%Y-%m-%d")
        ),
        dplyr::across(
          tidyselect::where(\(x) inherits(x, "POSIXt")),
          \(x) format(x, "%Y-%m-%d %H-%M-%S")
        )
      )
    
  }
  
  return(dataset_list_nako)
  
}

# ...
code_missing_values_for_nako <- function(dataset_list_nako, config) {
  # - null (could also be empty) in the values
  # - add a column with name <var_name>_m, where is null (or empty) when 
  # there's a value, and missing code, for the moment -1 everywhere
  
  if (!(length(config$nako$missing_codings) == 1
    & config$nako$missing_codings == "-1")) {
    stop("Sorry, I can only put -1 in missing-value columns in data sets ",
      "for NAKO. Please change the config file accordingly.")
  }
  
  part_ids <- c("Proband", "participant_id")
  
  for (nm in names(dataset_list_nako)) {
    
    ds_n <- names(dataset_list_nako[[nm]])
    
    ds_missings <- dataset_list_nako[[nm]] |> 
      dplyr::select(-any_of(part_ids))
    names(ds_missings) <- paste0(names(ds_missings), "_m")
    ds_missings <- ds_missings |> 
      dplyr::mutate(
        dplyr::across(
          .fns = \(x) 
          sapply(x, \(x1) if(is.na(x1)) { -1 } else { as.numeric(NA) }) |> 
            stats::setNames(NULL)
        )
      )
    
    ds_final <- dataset_list_nako[[nm]] |>
      dplyr::mutate(
        dplyr::across(
          .fns = \(x) 
          sapply(x, \(x1) if(is.na(x1)) { NA } else { x1 }) |> 
            stats::setNames(NULL)
        )
      ) |> 
      dplyr::bind_cols(ds_missings)
    
    ori_part_ids <- part_ids[part_ids %in% ds_n]
    ori_names <- ds_n[!ds_n %in% part_ids]
    
    col_ordered <- c(
      ori_part_ids,
      rep(NA, 2 * length(ori_names))
    )
    col_ordered[length(ori_part_ids) - 1 + 2 * (1:(length(ori_names)))] <- 
      ori_names
    col_ordered[is.na(col_ordered)] <- paste0(ori_names, "_m")
    
    ds_final <- ds_final |> dplyr::select(dplyr::all_of(col_ordered))
    dataset_list_nako[[nm]] <- ds_final
    
  }
  
  return(dataset_list_nako)  
  
}

# ...
move_participant_to_front_for_nako <- function(dataset_list_nako) {
  
  for (nm in names(dataset_list_nako)) {
    
    ds_n <- names(dataset_list_nako[[nm]])
    col_reordered <- c("Proband", ds_n[ds_n != "Proband"])  
    
    dataset_list_nako[[nm]] <- dataset_list_nako[[nm]] |> 
      dplyr::select(dplyr::all_of(col_reordered))
    
  }
  
  return(dataset_list_nako)
  
}

# ...
split_answers_in_questionnaires_for_nako <- function(dataset_list_nako, 
  questionnaire_col) {

  # Split the answers in questionnaires
  answers_questionnaires <- split(
    dataset_list_nako$answers, 
    f = dataset_list_nako$answers[[questionnaire_col]]
  )
  names(answers_questionnaires) <- standardize_questionnaire_names(
    names(answers_questionnaires)
  )
  
  # Replace the list of answers
  dataset_list_nako_s <- dataset_list_nako
  dataset_list_nako_s[["answers"]] <- NULL
  dataset_list_nako_s <- append(dataset_list_nako_s, answers_questionnaires)
  
  return(dataset_list_nako_s)
  
}


# ... 
standardize_questionnaire_names <- function(q_names) {  
  
  gsub("[^A-z0-9]", "_", q_names)
  
}

# ...
generate_meta_data_for_nako <- function(dataset_list_nako_s, 
  questionnaire_names, config, var_dict_nako) {
  
  metadata_nako <- tibble::tibble()
  
  for (nm in names(dataset_list_nako_s)) {
    
    types_titles <- get_titles_descritptions_for_nako(
      nm, config, questionnaire_names
    )
    dataset_type_title <- types_titles$dataset_type_title
    dataset_description <- types_titles$dataset_description
    
    # Meta-data of individual variables
    for(var in names(dataset_list_nako_s[[nm]])) {
      
      # Columns encoding missing values have names ending with "_m" and another
      # column with their names without the suffix "_m" is present in the data
      # set. 
      is_missing_col <- (substr(var, nchar(var)-1, nchar(var)) == "_m" 
        & substr(var, 1, nchar(var)-2) %in% names(dataset_list_nako_s[[nm]]))
      
      if (var == "Proband") {
        var_name_original <- "participant_id"
      } else if (!is_missing_col) {
        var_name_original <- var_dict_nako$original[var_dict_nako$nako == var]
      } else {
        var_name_original <- var_dict_nako$original[
          var_dict_nako$nako == substr(var, 1, nchar(var)-2)
        ]
      }
      
      if (is_missing_col) {
        
        # Title - missing
        var_title <- replace_null_default(
          config$variables[[var_name_original]]$title, 
          substr(var, 1, nchar(var)-2)
        )
        
        # Description - missing
        var_description <- paste0(
          "Missing: ", 
          replace_null_default(
            config$variables[[var_name_original]]$description,
            var_title
          )
        )
        
        # Adjust title - missing
        var_title <- paste0(
          "Missing: ",
          var_title
        )
        
        # Type - missing
        var_type <- "integer"
        
        # Scale level - missing  
        var_scale_level <- "nominal"
        
        # Units - missing
        var_unit <- NULL
        
        # Options - missing
        var_options <- config$nako$missing_options
        
      } else {
        
        # Title
        var_title <- replace_null_default(
          config$variables[[var_name_original]]$title, var
        )
        
        # Description
        var_description <- replace_null_default(
          config$variables[[var_name_original]]$description,
          var_title
        )
        
        # Type
        var_type <- config$variables[[var_name_original]]$type
        var_type <- ifelse(
          is.null(var_type),
          typeof(dataset_list_nako_s[[nm]][[var]]), 
          var_type
        )
        
        # Description: addendum for date-time variables
        if (var_type == "date_time" & config$convert_time_from_germany) {
          var_description <- paste0(
            var_description,
            ". Date and time assumed to have been measured in Germany and ",
            "converted to Coordinated Universal Time (UTC)."
          )
        }
        
        # Scale level
        var_scale_level <- config$variables[[var_name_original]]$scale_level    
        if (is.null(var_scale_level)) {
          if (config$nako$guess_scale_level) {
            var_scale_level <- guess_variable_scale_level(var_type)
          } else {
            stop("Please supply, in the config file, a scale level for variable ",
              var_name_original, ".")
          }
        }
        
        # Units
        var_unit <- config$variables[[var_name_original]]$unit
        
        # Options
        var_options <- config$variables[[var_name_original]]$options
        
      }
      
      # Check title length
      if (nchar(var_title) > 60) {
        stop("Variable titles for the NAKO should have 60 characters or less. ",
          "Since titles of variables for missing values have \"Missing: \" ",
          "added to the title of the original variable, NAKO variable titles ",
          "should have 51 or less characters. Please change, in the config ",
          "file, the title for variable ", var_name_original, ".")
      }
      
      # Convert type
      var_type <- convert_to_sql_type(var_type)
      
      # Bring all meta-data together
      metadata_nako_var <- tibble::tibble(
        `Abschnitt 1.Titel` = null_to_na(dataset_type_title),
        `Abschnitt 1.Beschreibung` = null_to_na(dataset_type_description),
        `Abschnitt 2.Titel` = null_to_na(dataset_title),
        `Abschnitt 2.Beschreibung` = null_to_na(dataset_description),
        Name = null_to_na(var),
        Titel = null_to_na(var_title),
        Beschreibung = null_to_na(var_description),
        Datentyp = null_to_na(var_type),
        Skalenniveau = null_to_na(var_scale_level),
        Einheit = null_to_na(var_unit),
        Optionen = null_to_na(var_options)
      )
      
      metadata_nako <- dplyr::bind_rows(metadata_nako,  metadata_nako_var)
      
    }
  }
  
  return(metadata_nako)
  
}

# ...
get_titles_descritptions_for_nako <- function(nm, config, questionnaire_names) {
  
  # For individual questionnaires, use the overall title and description
  # for all answers
  nm_dst <- ifelse(nm %in% questionnaire_names, "answers", nm)
  
  # Meta-data of type of data set
  dataset_type <- config$datasets[[nm_dst]]$dataset_type
  dataset_type_title <- ifelse(
    is.null(dataset_type),
    NULL,
    config$dataset_types[[dataset_type]]$title
  )
  
  if (!is.null(dataset_type_title)) {
    if (nchar(dataset_type_title) > 60) {
      stop("Data-set-type title should have 60 signs or less. Please ",
        "change, in the config file, the title for variable ", dataset_type, 
        ".")
    }
  }
  
  dataset_type_description <- 
    config$dataset_types[[dataset_type]]$description
  
  # Meta-data of data set
  dataset_title <- replace_null_default(
    config$datasets[[nm]]$title,
    nm
  )
  if (nchar(dataset_title) > 60) {
    stop("Data-set title should have 60 signs or less. Please change, ",
      "in the config file, the title for data set ", nm, ".")
  }
  
  dataset_description <- config$datasets[[nm_dst]]$description
  
  return(
    list(
      dataset_type_title = dataset_type_title, 
      dataset_description = dataset_description
    )
  )
  
}

# ...
convert_to_sql_type <- function(var_type) {
  # Reference: https://mariadb.com/kb/en/data-types/
  
  if (is.null(var_type)) {
    
    sql_type <- NULL
    
  } else if (var_type %in% c("boolean", "logical")) {
    
    sql_type <- "BOOLEAN"
    
  } else if (var_type == "character") {
    
    sql_type <- "CHAR"
    
  } else if (var_type == "date") {
    
    sql_type <- "DATE"
    
  } else if (var_type == "date_time") {
    
    sql_type <- "DATETIME"
    
  } else if (var_type %in% c("float", "double")) {
    # R only has double precision
    
    sql_type <- "DOUBLE"
    
  } else if (var_type == "integer") {
    
    sql_type <- "INT"
    
  } else { 
    
    stop("I don't know how to convert variable type ", var_type, " to an SQL ",
      "type. Please change or add a type in the config file.")
    
  }
  
  return(sql_type)  
  
}

# ...
guess_variable_scale_level <- function(var_type) {
  
  
  scale_level <- dplyr::case_when(
    var_type %in% c("boolean", "logical", "character") ~ "nominal",
    var_type == "integer" ~ "ordinal",
    var_type %in% c("double", "float", "date", "date_time") ~ "metrisch",
    TRUE ~ "Text"
  )
  
  return(scale_level)
  
}


# ...
export_for_nako <- function(for_nako, config) {
  
  exp_dir <- paste0(
    config$nako$export_path,
    "/",
    "NAKO-", 
    config$nako$application_number
  )
  dir.create(exp_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Meta-data
  write_csv_for_nako(
    for_nako$metadata, paste0(exp_dir, "/metadata.csv"), "", TRUE
  )

  # Questionnaire names
  write.csv(
    data.frame(questionnaire = for_nako$questionnaire_names), 
    paste0(exp_dir, "/questionnaire-names.csv"),
    row.names = FALSE
  )
    
  # Data
  exp_data_dir <- paste0(exp_dir, "/data")
  dir.create(exp_data_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (nm in names(for_nako$data)) {
    
    # Data-set type
    types_titles <- get_titles_descritptions_for_nako(
      nm, config, for_nako$questionnaire_names
    )
    dataset_type_title <- types_titles$dataset_type_title
    
    if (!is.null(dataset_type_title)) {
      dst_dir <- paste0(
        exp_data_dir,
        "/",
        gsub("[^A-z0-9]", "_", dataset_type_title)
      )
      dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
    } else {
      dst_dir <- exp_data_dir
    }
    
    # Data set
    dataset_title <- replace_null_default(
      config$datasets[[nm]]$title,
      nm
    )
    ds_file <- paste0(
      dst_dir,
      "/",
      gsub("[^A-z0-9]", "_", dataset_title), 
      ".csv"
    )
    
    # Add quotation marks manually so as to be able to print null without 
    # quotation marks.
    ds <- for_nako$data[[nm]] |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::where(is.character),
          \(x) ifelse(is.na(x), as.character(NA), paste0("\"", x, "\""))
        )
      )
    
    write_csv_for_nako(ds, ds_file, "null", FALSE)
    
  }
  
}

write_csv_for_nako <- function(df, file_path, na_encode, quote_added) {
  
  write.table(
    df,
    file = file_path,
    quote = quote_added, 
    sep = ";",
    eol = "\n", 
    na = na_encode, 
    dec = ".", 
    row.names = FALSE,
    col.names = TRUE,
    qmethod = "double",
    fileEncoding = "UTF-8"
  )
  
}