# ...
arrange_codebook_to_table <- function(codebook) {

  codebook_table <- NULL
  for (nm in names(codebook)) {
    for (qu in codebook[[nm]]$questions) {

      question_overall <- tibble::tibble(
        questionnaire_name = codebook[[nm]]$name,
        questionnaire_version = get_questionnaire_version(nm),
        question_text = as.character(null_to_na(qu$text)),
        question_position = as.integer(null_to_na(qu$position)),
        is_mandatory = as.logical(null_to_na(qu$is_mandatory)),
        expires_after_days = as.integer(null_to_na(qu$expires_after_days)),
        finalises_after_days = as.integer(null_to_na(qu$finalises_after_days))
      )

      if (length(qu$answer_options) == 0) {

        question_overall |>
          dplyr::mutate(
            answer_level = as.integer(NA),
            follow_up_question = as.character(NA),
            answer_position = as.integer(NA),
            answer_type_id = as.character(NA),
            answer_values = as.character(NA),
            answer_values_code = as.character(NA)
          )

      } else {

        for (i_ao in 1:length(qu$answer_options)) {

          ao <- qu$answer_options[[i_ao]]

          question <- question_overall |>
            dplyr::mutate(
              answer_level = i_ao,
              follow_up_question = as.character(null_to_na(ao$text)),
              answer_position = as.integer(null_to_na(ao$position)),
              answer_type_id = as.character(null_to_na(ao$answer_type_id)),
              answer_values = as.character(
                null_to_na(
                  multiple_values_to_string(ao$values)
                )
              ),
              answer_values_code = as.character(
                null_to_na(
                  multiple_values_to_string(ao$values_code)
                )
              )
            )

          codebook_table <- codebook_table |> dplyr::bind_rows(question)

        }

      }

    }

  }

  return(codebook_table)

}

# ...
get_questionnaire_version <- function(nm) {

  if (grepl("(\\([0-9]+\\))$", nm)) {

    version <- as.integer(
      gsub(")", "", tail(strsplit(nm, "\\(")[[1]], 1))
    ) + 1

  } else {

    version <- 1L

  }

  return(version)

}

# ...
multiple_values_to_string <- function(values) {

  if (length(values) == 0) {

    val_str <- as.character(NA)

  } else if (length(values) == 1) {

    val_str <- paste0("\"", values, "\"")

  } else {

    val_str <- paste0("[\"", paste(values, collapse = "\",\""), "\"]")
  }

  return(val_str)

}

# ...
add_question_id <- function(codebook_table) {

  codebook_table |>
    dplyr::mutate(
      question_single = paste0(questionnaire_name, "_v", questionnaire_version,
        "_f", question_position, "_", answer_position)
    ) |> 
    dplyr::relocate(question_single)

}

