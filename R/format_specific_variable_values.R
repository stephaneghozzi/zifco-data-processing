# ...
format_specific_variable_values <- function(dataset_name, dataset_df) {

  cleaned_dataset <- dataset_df

  # Initial quantity is of the form "12,34 g", we want "12.34". Special case
  # where "99,00 g" means "too high"; the latter is replaced with a missing
  # value.
  var_names <- "initial_quantity"
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) {
          x <- gsub(",", ".", gsub(" g", "", x))
          dplyr::if_else(
            x == "99.00",
            as.character(NA),
            x
          )
        }
      )
    )

  # Remaining quantity is of the form "1234,56 µl", we want "1234.56".
  var_names <- "remaining_quantity"
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) gsub(",", ".", gsub(" µl", "", x))
      )
    )

  # Concentration is of the form "1,23 xE06", we want first "1.23e06", which
  # be converted to a number when converting type.
  var_names <- "concentration"
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) gsub(",", ".", gsub(" xE", "e", x))
      )
    )

  # ...
  var_names <- c("consent_blood_sample_collection",
    "consent_result_communication", "consent_sample_collection",
    "test_participant")
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) dplyr::case_when(
          tolower(x) == "ja" ~ TRUE,
          tolower(x) == "nein" ~ FALSE,
          TRUE ~ NA
        )
      )
    )

  # ...
  var_names <- c("collection_date", "delivery_date", "analysis_date",
    "reporting_date", "questionnaire_date", "answer_date")
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) ifelse(
          gsub("[0-9]", "", x) == ".., :",
          paste0(x, ":00"),
          x
        )
      )
    )

  # ...
  var_names <- "answer_is_mandatory"
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) dplyr::case_when(
          x == "t" ~ TRUE,
          x == "f" ~ FALSE,
          as.logical(x) ~ TRUE,
          !as.logical(x) ~ FALSE,
          TRUE ~ NA
        )
      )
    )


  # Check that various ID variables are well-formatted. If not, set to missing.
  var_names <- c("participant_id", "participant_id_hub", "sample_id",
    "bakt_sample_id")
  for (vn in var_names) {
    cleaned_dataset <- cleaned_dataset |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(vn),
          \(x) format_ID_variables(x, n)
        )
      )
  }
  
  # Format to match code book
  var_names <- "answer_values"
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) ifelse(x %in% c("[]", "{}"), as.character(NA), x)
      )
    )
  
  var_names <- "answer_values_code"
  cleaned_dataset <- cleaned_dataset |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(var_names),
        \(x) {
          y <- stringr::str_replace_all(
            x, 
            c( '","' = ",", "," = '","', "\\{" = '\\[\"', "\\}" = '\"\\]', 
              '\\[""\\]' = "\\[\\]")
          )
          ifelse(y == "[]", as.character(NA), y)
        }
      )
    )

  return(cleaned_dataset)

}

format_ID_variables <- function(x, var_name) {

  x <- tolower(x)

  if (var_name == "participant_id") {

    correct_format <- "^(l3pia)([0-9]{9})$"

  } else if (var_name == "participant_id_hub") {

    correct_format <- "^(hzif)([0-9]{3,4})$"

  } else if (var_name %in% c("sample_id", "bakt_sample_id")) {

    # Keep samples "RSIST-" and "RFEE-" from other studies to make sure
    # corresponding entires are completely removed from data sets.
    correct_format <- paste0(
      "(^([0-9]{9,10})$)",
      "|",
      "(^(na)([0-9]{10})$)",
      "|",
      "(^(zifco-)([0-9]{10})$)",
      "|",
      "(^(rsist-|rfee-))"
    )
  }

  x[!grepl(correct_format, x)] <- as.character(NA)

  return(x)

}

