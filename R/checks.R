# ...
check_variable_names <- function(variable_names_count,
  variables_dictionary) {

  variable_names_checks <- ""

  # Check whether variables names have been explicitly translated
  variable_names <- sort(unique(variable_names_count$variable_name))

  if (all(variable_names %in% variables_dictionary$original)) {

    variable_names_checks <- paste0(
      variable_names_checks,
      "All variable names have been explicitly considered in the ",
      "configuration file.\n\n"
    )

  } else {

    implicit_var <- paste0(
      "\"",
      paste(
        variable_names[
          !variable_names %in% variables_dictionary$original
        ],
        collapse = '","'
      ),
      "\""
    )

    variable_names_checks <- paste0(
      variable_names_checks,
      "The following variables haven't been explicitly considered in the ",
      "configuration file, default standardization will be applied to their ",
      "names: ",
      implicit_var,
      "\n\n"
    )

  }

  # Check whether names appear multiple times across data sets
  overall_multiples <- variable_names_count |> dplyr::filter(overall_count > 1)


  if (nrow(overall_multiples) == 0) {

    variable_names_checks <- paste0(
      variable_names_checks,
      "Each variable name appears only once within and across data sets.\n\n"
    )

  } else {

    if (any(overall_multiples$intra_count > 1)) {

      intra_multiples <- overall_multiples |>
        dplyr::filter(intra_count > 1) |>
        dplyr::pull(variable_name) |>
        paste(collapse = "\", \"") |>
        (\(x) paste0("\"", x, "\""))()

      variable_names_checks <- paste0(
        variable_names_checks,
        "WARNING: The following variables apper many times within data sets: ",
        intra_multiples,
        ". See below for details.\n\n"
      )

    } else {

      variable_names_checks <- paste0(
        variable_names_checks,
        "No variable name appears multiple times within the same data set.\n\n"
      )

    }

    variable_names_checks <- paste0(
      variable_names_checks,
      "The following variable names appear overall many times accross data ",
      "sets:\n\n",
      describe_overall_multiplicity(overall_multiples),
      "\n\n"
    )

  }

  return(variable_names_checks)

}

# ...
describe_overall_multiplicity <- function(overall_multiples) {

  overall_multiplicity_message <- overall_multiples |>
    dplyr::arrange(variable_name) |>
    dplyr::mutate(
      message_1 = paste0(
        intra_count,
        " time(s) in ",
        dataset
      )
    ) |>
    dplyr::group_by(variable_name) |>
    dplyr::summarize(
      datasets = sort(paste(dataset, collapse = ", ")),
      n_ds = length(dataset),
      overall_count = unique(overall_count),
      message = paste(message_1, collapse = ", "),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      message = paste0(
        "- \"",
        variable_name,
        "\" appears overall ",
        overall_count,
        " times in ",
        n_ds,
        " data set(s) (",
        message,
        ")"
      )
    ) |>
    dplyr::pull(message) |>
    paste(collapse = "\n")

  return(overall_multiplicity_message)

}

# ...
check_pia_empty_answers <- function(answers) {

  # answers <- answers_pre_op

  p_empty_a <- sum(is.na(answers$answer)) / nrow(answers)
  p_empty_ad <- sum(is.na(answers$answer_date)) / nrow(answers)

  empty_q <- answers[answers$answer_values == "[]",]
  p_empty_q <- nrow(empty_q) / nrow(answers)
  p_empty_q_filled_answer_date <-
    sum(!is.na(empty_q$answer_date)) / nrow(empty_q)
  p_empty_q_filled_answer <- sum(!is.na(empty_q$answer)) / nrow(empty_q)
  p_empty_full <- (
    sum(is.na(empty_q$answer_date) & is.na(empty_q$answer))
    / nrow(answers)
  )

  pia_empty_answers_checks <- list(
    p_empty_q = p_empty_q,
    p_empty_q_filled_answer_date = p_empty_q_filled_answer_date,
    p_empty_q_filled_answer = p_empty_q_filled_answer,
    p_empty_full = p_empty_full,
    p_empty_a = p_empty_a,
    p_empty_ad = p_empty_ad
  )

  pia_empty_answers_checks <- lapply(
    pia_empty_answers_checks, \(x) percentage(x, 2)
  )

  return(pia_empty_answers_checks)

}

# ...
check_pia_multiple_answers <- function(answers) {

  answ_short_nr <- annotate_answer_multiplicity(answers)

  mult_quest <- answ_short_nr |>
    dplyr::group_by(question_single, participant_id, questionnaire_date) |>
    dplyr::summarize(
      numbers = paste(question_number, collapse = ", "),
      diff_a_dates = suppressWarnings(
        max(answer_date, na.rm = TRUE) - min(answer_date, na.rm = TRUE)
      ),
      min_nr = suppressWarnings(min(question_number, na.rm =TRUE)),
      max_nr = suppressWarnings(max(question_number, na.rm =TRUE)),
      mult_l = length(question_single),
      expected = numbers %in% c("0", "1, 2", "2, 1"),
      .groups = "drop"
    ) |>
    dplyr::arrange(question_single, participant_id, questionnaire_date)

  c_expected_mult_l <- mult_quest |>
    dplyr::count(expected, mult_l, name = "freq") |>
    dplyr::mutate(perc = percentage(freq / nrow(mult_quest), 2))

  p_not_exp <- sum(c_expected_mult_l$perc[!c_expected_mult_l$expected])

  p_not_exp_mult2 <- sum(
    c_expected_mult_l$perc[
      !c_expected_mult_l$expected & c_expected_mult_l$mult_l > 1
    ]
  )

  pia_multiple_answers_checks <- list(
    p_not_exp = p_not_exp,
    p_not_exp_mult2 = p_not_exp_mult2
  )
  return(pia_multiple_answers_checks)

}

# ...
check_unambiguousness_matching_ids <- function(dataset_list, config) {

  if (!"ids_lookup" %in% names(dataset_list)) {

    dataset_list<- create_sample_participant_lookups(dataset_list, config)
    
  }
  ids_lookup <- dataset_list$ids_lookup

  multiplicity_matching <- compute_matching_multiplicity(ids_lookup)
  mm_samp_part <- multiplicity_matching[["sample_id"]][["participant_id"]]

  # Samples that are not matched to a participant
  no_match_sample_part <- mm_samp_part |>
    dplyr::filter(multiplicity == 0) |>
    dplyr::pull(sample_id)

  # Samples that are matched to many participants
  multiple_match_sample_part <- ids_lookup |>
    dplyr::filter(
      sample_id %in% mm_samp_part$sample_id[mm_samp_part$multiplicity > 1]
    ) |>
    dplyr::arrange(sample_id)

  # In which data sets do the samples with multiple participants appear
  # (directly or via the HUB participant ID)?
  id_variables <- c("participant_id_hub", "sample_id")
  if (nrow(multiple_match_sample_part) > 0) {

    mm_samp_part_datasets <- list()
    for (ds in names(dataset_list)) {
      for (nm in id_variables) {
        if (nm %in% names(dataset_list[[ds]])) {

          mm_samp_part_datasets[[ds]] <- mm_samp_part_datasets[[ds]] |>
            dplyr::bind_rows(
              dataset_list[[ds]] |>
                dplyr::filter(
                  .data[[nm]] %in% multiple_match_sample_part[[nm]]
                ) |>
                dplyr::select(dplyr::any_of(id_variables)) |>
                dplyr::mutate(dataset = ds)
            )

        }
      }
    }
    mm_samp_part_datasets <- dplyr::bind_rows(mm_samp_part_datasets) |>
      dplyr::arrange(sample_id)

  } else {

    mm_samp_part_datasets <- tibble::tibble()

  }

  ambiguousness_matching <- list(
    no_match_sample_part = no_match_sample_part,
    multiple_match_sample_part = multiple_match_sample_part,
    mm_samp_part_datasets = mm_samp_part_datasets
  )

  return(ambiguousness_matching)

}

# ...
compute_matching_multiplicity <- function(ids_lookup) {

  id_variables_pairs <- combn(names(ids_lookup), 2)
  multiplicity_matching <- list()
  for (i in 1:ncol(id_variables_pairs)) {

    vp <- id_variables_pairs[, i]
    for(i_order in 0:1) {

      v1 <- vp[1 + i_order]
      v2 <- vp[2 - i_order]
      multiplicity_matching[[v1]][[v2]] <- ids_lookup |>
        dplyr::select(dplyr::all_of(c(v1, v2))) |>
        dplyr::distinct() |>
        dplyr::group_by(dplyr::across(dplyr::all_of(v1))) |>
        dplyr::summarize(
          multiplicity = length(dplyr::cur_data()[!is.na(dplyr::cur_data())])
        )
    }

  }

  return(multiplicity_matching)

}

# ...
summarize_dataset_percentage <- function(x, var_name) {


  paste(
    sapply(
      names(dataset_list),
      \(nm) paste0(
        nm,
        ": ",
        ifelse(
          var_name %in% names(dataset_list[[nm]]),
          paste0(percentage(x[[nm]], 2), "%"),
          "N.A."
        )
      )
    ),
    collapse = ", "
  )

}

# ...
check_variable_formats <- function(dataset_list_raw) {

  var_names_clean <- c("participant_id", "participant_id_hub", "sample_id")
  var_names_ori <- lapply(
    var_names_clean,
    \(vn) config$variables[[vn]]$original
  )
  names(var_names_ori) <- var_names_clean
  cvf <- list()
  for (vn in names(var_names_ori)) {
    all_ids <- sort(
      unique(
        unlist(
          sapply(
            var_names_ori[[vn]],
            \(vn_ori) unlist(sapply(dataset_list_raw, \(x) unique(x[[vn_ori]])))
          )
        )
      )
    )
    cvf[[vn]] <- list(
      all_ids = all_ids,
      all_ids_formatted = format_ID_variables(all_ids, vn)
    )

  }

  return(cvf)

}
