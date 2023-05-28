# ...
dataset_operations <- function(dataset_list, config) {

  answer_vars <- names(dataset_list$answers)
  
  dataset_list <- unify_sample_id_names(dataset_list)

  dataset_list <- adhoc_arrangements(dataset_list)

  dataset_list <- remove_test_participants(dataset_list)

  dataset_list <- remove_irrelevant_variables(dataset_list, config)
  
  dataset_list <- remove_specific_entries(dataset_list, config)

  dataset_list <- remove_irrelevant_samples(dataset_list)

  if ("answers" %in% names(dataset_list)) {

    dataset_list$answers <- dataset_list$answers |>
      keep_valid_answer_from_multiple() |>
      dplyr::distinct()

  }

  if ("answers_backup" %in% names(dataset_list)) {

    dataset_list$answers_backup <- dataset_list$answers_backup |>
      select_relevant_questionnaires() |>
      format_to_pia_export(answer_vars) |>
      keep_valid_answer_from_multiple() |>
      dplyr::distinct()

  }

  if (all(c("answers", "answers_backup") %in% names(dataset_list))) {

    dataset_list$answers <- dataset_list$answers |>
      dplyr::bind_rows(dataset_list$answers_backup) |>
      dplyr::distinct()

    dataset_list$answers_backup <- NULL

  }
  
  if (all(c("answers", "pia_codebook") %in% names(dataset_list))) {

    dataset_list$answers <- dataset_list$answers |>
      dplyr::select(-answer_values, -answer_values_code) |> 
      dplyr::filter(
        question_single %in% dataset_list$pia_codebook$question_single
      )

  }
  
  dataset_list<- create_sample_participant_lookups(dataset_list, config)


  if (all(c("pbmc_1", "pbmc_2") %in% names(dataset_list))) {

    dataset_list$pbmc <- merge_pbmc_datasets(
      dataset_list$pbmc_1, dataset_list$pbmc_2
    )
    dataset_list$pbmc_1 <- NULL
    dataset_list$pbmc_2 <- NULL

  }

  if (all(c("cpt_hub", "cpt_pia") %in% names(dataset_list))) {

    dataset_list$cpt <- merge_cpt_datasets(
      dataset_list$cpt_hub, dataset_list$cpt_pia
    )
    dataset_list$cpt_hub <- NULL
    dataset_list$cpt_pia <- NULL

  }

  if (config$remove_entries_ambiguous_rna_samples) {
    
    dataset_list <- remove_entries_ambiguous_rna_samples(dataset_list)
    
  }
  
  dataset_list <- match_participants_to_samples(dataset_list)


  return(dataset_list)

}

# ...
adhoc_arrangements <- function(dataset_list) {

  # There was a mistake and one participant
  # was entered twice: delete the entry for that participant which has a missing
  # `delivery_date`.
  if ("cpt_hub" %in% names(dataset_list)) {

    dataset_list$cpt_hub <- dataset_list$cpt_hub |>
      dplyr::filter(!(participant_id_hub == "hzif0386" & is.na(delivery_date)))

  }

  return(dataset_list)

}

# ...
remove_test_participants <- function(dataset_list) {

  if ("consent" %in% names(dataset_list)) {

    test_participant_ids <- dataset_list$consent |>
      dplyr::filter(test_participant) |>
      dplyr::pull(participant_id) |>
      unique()

    for (nm in names(dataset_list)[names(dataset_list) != "consent"]) {
      if ("participant_id" %in% names(dataset_list[[nm]])) {

         dataset_list[[nm]] <- dataset_list[[nm]] |>
           dplyr::filter(!participant_id %in% test_participant_ids)

      }
    }

  }

  return(dataset_list)

}

# ...
remove_irrelevant_variables <- function(dataset_list, config) {

  remove_vars <- config$remove_variables

  for (nm in names(dataset_list)) {
    dataset_list[[nm]] <- dataset_list[[nm]] |>
      dplyr::select(-dplyr::any_of(remove_vars))
  }

  return(dataset_list)

}

# ...
remove_specific_entries <- function(dataset_list, config) {
  
  for (vn in names(config$remove_entries_with_values)) {
    for (nm in names(dataset_list)) {
      ds <- dataset_list[[nm]]
      
      if (vn %in% names(ds)) {
        
        dataset_list[[nm]] <- ds |>
          dplyr::filter(
            !.data[[vn]] %in% config$remove_entries_with_values[[vn]]
          )
      }
    }
  }
  
  return(dataset_list)
  
}

#...
keep_valid_answer_from_multiple <- function(answers) {

  answers <- answers |>
    annotate_answer_multiplicity() |>
    dplyr::group_by(question_id, participant_id, questionnaire_date) |>
    dplyr::mutate(
      question_number_max = suppressWarnings(max(question_number, na.rm = TRUE))
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(question_number == question_number_max) |>
    dplyr::select(-question_id, -question_number, -question_number_max)

  return(answers)

}

# ...
annotate_answer_multiplicity <- function(answers) {

  answers <- answers |>
    dplyr::mutate(
      question_single = ifelse(
        grepl("(_a[0-9]*)$", question_id),
        sapply(
          question_id,
          \(x) stringr::str_split_1(x, "_a")[
              1:(length(stringr::str_split_1(x, "_a")) - 1)
          ]
        ),
        question_id
      ),
      question_number = ifelse(
        grepl("(_a[0-9]*)$", question_id),
        sapply(question_id, \(x) tail(stringr::str_split_1(x, "_a"), 1)),
        0
      )
    )

  return(answers)

}

# ...
select_relevant_questionnaires <- function(answers_backup) {
  # Only some questionnaires from backup are relevant.

  keep_questionnaires <- c("Spontanmeldung", "Regionsfragebogen",
    "Symptome Atemwege", "Spontanmeldung: Symptome Atemwege")

  answers_backup <- answers_backup |>
    dplyr::filter(questionnaire_name %in% keep_questionnaires)

  return(answers_backup)

}

# ...
format_to_pia_export <- function(answers_backup, answer_vars) {

  answers_backup <- answers_backup |>
    dplyr::mutate(
      question_id = paste0(questionnaire_name, "_v", questionnaire_version,
        "_f", question_position, "_", answer_position),
      question_id = paste0(
        question_id,
        ifelse(is.na(question_number), "", paste0("_a", question_number))
      ),
      answer_date = dplyr::case_when(
        question_number == 1 ~ date_of_release_v1,
        question_number == 2 ~ date_of_release_v2,
        TRUE ~ as.POSIXct(NA)
      )
    ) |>
    dplyr::rename(questionnaire_date = date_of_issue) |>
    dplyr::select(dplyr::any_of(answer_vars))

  return(answers_backup)

}

# ...
unify_sample_id_names <- function(dataset_list) {

  for (nm in names(dataset_list)) {
    if (all(c("sample_id", "bakt_sample_id") %in% names(dataset_list[[nm]]))) {

      ds1 <- dataset_list[[nm]] |> dplyr::select(-bakt_sample_id)
      ds2 <- dataset_list[[nm]] |>
        dplyr::select(-sample_id) |>
        dplyr::rename(sample_id = bakt_sample_id)

      dataset_list[[nm]] <- dplyr::bind_rows(ds1, ds2)
      
      # Duplicates might have been introduced if, e.g.,  different 
      # `bakt_sample_id` corresponded to otherwise identical data. 
      dataset_list[[nm]] <- dplyr::distinct(dataset_list[[nm]])

    } else if ("bakt_sample_id" %in% names(dataset_list[[nm]])) {

      dataset_list[[nm]] <- dataset_list[[nm]] |>
        dplyr::rename(sample_id = bakt_sample_id)

    }

  }


  return(dataset_list)

}

# ...
remove_irrelevant_samples <- function(dataset_list) {

  for (nm in names(dataset_list)) {

    # Sometimes a sample ID for a participant would be re-used for
    # a second participant if, contrary what had been foreseen, a sample was not
    # taken for the first participant.
    if (all(c("sample_id", "sample_status") %in% names(dataset_list[[nm]]))) {

      dataset_list[[nm]] <- dataset_list[[nm]] |>
        dplyr::add_count(sample_id, name = "sample_mult") |>
        dplyr::filter(
          is.na(sample_status)
          | !(sample_status == "nicht genommen" & sample_mult > 1)
        ) |>
        dplyr::select(-sample_mult)

    }

    # Remove samples from other studies that shouldn't be here.
    remove_samples <- "^(rsist-|rfee-)"
    if ("sample_id" %in% names(dataset_list[[nm]])) {

      dataset_list[[nm]] <- dataset_list[[nm]] |>
        dplyr::filter(!grepl(remove_samples, tolower(sample_id)))

    }

  }

  return(dataset_list)

}

# ...
merge_cpt_datasets <- function(cpt_hub, cpt_pia) {

  cpt <- dplyr::full_join(cpt_hub, cpt_pia, by = "sample_id")

  return(cpt)

}

# ...
merge_pbmc_datasets <- function(pbmc_1, pbmc_2) {

  common_pbmc_vars <- intersect(names(pbmc_1), names(pbmc_2))
  pbmc <- dplyr::full_join(pbmc_1, pbmc_2 , by = common_pbmc_vars)

  return(pbmc)

}

# ...
create_sample_participant_lookups <- function(dataset_list, config) {

  id_variables <- c("participant_id", "participant_id_hub", "sample_id")
  unique_id_value_pairs <- get_two_way_matches(dataset_list, id_variables)
  
  # Method 1: build all pairs sample-participant
  dataset_list$ids_lookup_1 <- get_all_sample_part_pairs(unique_id_value_pairs)
  
  # Method 2: build triplets sample-participant-HUB
  dataset_list$ids_lookup_2 <- get_samp_part_hub_triplets(unique_id_value_pairs)
  
  # Should the sample-participant lookup used be the safe matching obtained by 
  # sample-participant pairs directly observed in the data sets, or the larger
  # one obtained via HUB participant IDs.
  dataset_list$ids_lookup <- dataset_list$ids_lookup_1
  if (config$match_via_hub) {
    dataset_list$ids_lookup <- dataset_list$ids_lookup |> 
      dplyr::filter(!samp_part_via_hub)
  }
  dataset_list$ids_lookup <- dataset_list$ids_lookup |>  
    dplyr::select(-samp_part_via_hub) |> 
    dplyr::distinct() 

  return(dataset_list)

}

# ...
get_two_way_matches <- function(dataset_list, id_variables) {
  
  # Build lookups of ID pairs, keeping missing values
  id_variables_pairs <- combn(id_variables, 2)
  unique_id_value_pairs <- list()
  j <- 1
  for (i in 1:ncol(id_variables_pairs)) {

    vp <- id_variables_pairs[, i]

    relevant_datasets <- names(dataset_list)[
      sapply(
        dataset_list,
        \(ds) all(vp %in% names(ds))
      )
    ]

    if(length(relevant_datasets) > 0) {
    
      unique_id_value_pairs[[j]] <- lapply(
        relevant_datasets,
        \(nm) dataset_list[[nm]] |>
          dplyr::select(dplyr::all_of(vp))
      ) |>
        dplyr::bind_rows() |>
        dplyr::distinct()
    
      j <- j + 1
    
    }

  }
  
  return(unique_id_value_pairs)
  
}

# ...
get_all_sample_part_pairs <- function(unique_id_value_pairs) {
  
  # Look at all ways of building sample-participant matches, ignoring missing
  # values: directly, over sample-HUB-participant match, or 
  # sample-HUB-sample-participant match. (Even though all HUB-participant pairs 
  # come together with a sample ID, it could be the two samples are linked to 
  # the same HUB but only the first is linked to a participant, in which case we
  # can link the second over the associated HUB, then the first sample, then its 
  # participant.)
  
  sp_d <- NULL
  ph_d <- NULL
  sh_d <- NULL
  for (i in 1:length(unique_id_value_pairs)) {
    uivp <- unique_id_value_pairs[[i]]
    
    if (all(c("participant_id", "sample_id") %in% names(uivp))) {
      sp_d <- uivp
    }
    
    if (all(c("participant_id", "participant_id_hub") %in% names(uivp))) {
      ph_d <- uivp
    }
    
    if (all(c("sample_id", "participant_id_hub") %in% names(uivp))) {
      sh_d <- uivp
    }
    
  }
  
  if (!is.null(sh_d) & !is.null(ph_d)) {
    
    sp_h <- dplyr::inner_join(
      sh_d, 
      ph_d, 
      by = "participant_id_hub",
      na_matches = "never"
  ) |> 
    dplyr::select(-participant_id_hub)
    
  } else {
    
    sp_h <- NULL
    
  }
  
  if (!is.null(sh_d) & !is.null(sp_d)) {
    
    sp_hs <- dplyr::inner_join(
      sh_d,
      sh_d,
      by = "participant_id_hub", 
      suffix = c("", "_tmp"),
      na_matches = "never"
    ) |> 
      dplyr::select(-participant_id_hub) |> 
      dplyr::inner_join(
        sp_d,
        by = c("sample_id_tmp" = "sample_id"),
        na_matches = "never"
      ) |> 
      dplyr::select(-sample_id_tmp)
    
  } else {
    
    sp_hs <- NULL
    
  }
  
  if (!is.null(sp_d)) {
  
    ids_lookup <- dplyr::bind_rows(
      sp_d,
      sp_h,
      sp_hs
    ) |> 
      dplyr::distinct() |> 
      dplyr::relocate(participant_id, sample_id) |> 
      dplyr::arrange(participant_id, sample_id) |> 
      dplyr::left_join(
        sp_d |> dplyr::mutate(samp_part_via_hub = FALSE),
        by = c("participant_id", "sample_id")
      ) |> 
      dplyr::mutate(
        samp_part_via_hub = ifelse(
          is.na(samp_part_via_hub), 
          TRUE, 
          samp_part_via_hub
        )
      )
  
  } else {
    
    ids_lookup <- NULL
    
  }
  
  return(ids_lookup)
  
}  

# ...
get_samp_part_hub_triplets <- function(unique_id_value_pairs) {
  
  id_variables <- unique(c(sapply(unique_id_value_pairs, \(x) names(x))))
  
  # If there's only two variables with macthing, don't bother
  if (length(id_variables) == 2) {
    
    ids_lookup <- NULL
    
  } else {
  
    pairs_of_variable_pairs <- combn(1:length(unique_id_value_pairs), 2)
    ids_lookup_list <- list()
    for (j in 1:ncol(pairs_of_variable_pairs)) {
  
      pvp <- pairs_of_variable_pairs[, j]
      ds1 <- unique_id_value_pairs[[pvp[1]]]
      ds2 <- unique_id_value_pairs[[pvp[2]]]
      common_var <- intersect(names(ds1), names(ds2))
  
      ids_lookup_list[[j]] <- dplyr::full_join(
        ds1, 
        ds2, 
        by = common_var,
        na_matches = "never"
      )
  
    }
   
    # Keep meaningful triplets
    ids_lookup <- ids_lookup_list |>
      dplyr::bind_rows() |>
      dplyr::distinct()
    
    id_variables_pairs <- combn(id_variables, 2)
    for (j in 1:ncol(id_variables_pairs)) {
  
      v_pair <- id_variables_pairs[, j]
      v_test <- id_variables[!id_variables %in% v_pair]
  
      ids_lookup <- keep_meaningful_triplets(ids_lookup, v_pair, v_test)
  
    }
    ids_lookup <- dplyr::distinct(ids_lookup)
  }
  
  return(ids_lookup)
  
}

# ...
keep_meaningful_triplets <- function(ids_lookup, v_pair, v_test) {

  # Given a pair of values x, y of the two variables in `v_pair`:
  # 
  # - if all corresponding values z_1, ..., z_n of variable `v_test` are 
  # missing, then `NA` is indeed the value of `v_test` to be associated with 
  # x, y : `keep` is `TRUE` for all values z's of `v_test`;
  # - if any of the corresponding values z_1, ..., z_n of variable `v_test` is
  # not missing, then keep only the non-missing z's: keep is `TRUE` for the 
  # non-missing values, and `FALSE` for the missing values;
  # - of the initial lookup, only the distinct entries with `keep` are kept.
  # 
  # N.B.1. it can happen that x or y is `NA` and that at the end two triplets are 
  # kept of the form x1-y1-z1 and x1-NA-z1, although we don't want to keep the
  # second; but the second triplet will be removed when the second variable is 
  # tested (and can't appear here if it has already been tested) 
  # 
  # N.B.2. the triplet NA-NA-NA should be removed beforehand if not desired,
  # e.g., via ids_lookup <- ids_lookup 
  #             |> dplyr::filter(dplyr::if_any(.fns = ~ !is.na(.)))
  
  # # Tests:
  # v_pair <- c("v1", "v2")
  # v_test <- "v3"
  # ids_lookup <- tibble::tibble(
  #   v1 = as.integer(c(rep(1:6, each = 3), 6, NA)),
  #   v2 = as.integer(c(rep(1:6, each = 3), NA, 6)),
  #   v3 = as.character(c(
  #     NA, NA, NA,
  #     NA, NA, 1,
  #     NA, 1, 1,
  #     NA, 1, 2,
  #     1, 2, 3,
  #     1, 2, 2,
  #     1, 1
  #   ))
  # )
  # ids_lookup_expected <- tibble::tibble(
  #   v1 = as.integer(c(1, 2, 3, 4, 4, 5, 5, 5, 6, 6, 6, NA)),
  #   v2 = as.integer(c(1, 2, 3, 4, 4, 5, 5, 5, 6, 6, NA, 6)),
  #   v3 = as.character(c(
  #     NA,
  #     1,
  #     1,
  #     1, 2,
  #     1, 2, 3,
  #     1, 2,
  #     1, 1
  #   ))
  # )
  # identical(ids_lookup, ids_lookup_expected)
  
  ids_lookup <- ids_lookup |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(v_pair)
        )
      ) |>
      dplyr::mutate(
        keep = !is.na(.data[[v_test]]) | all(is.na(.data[[v_test]]))
      ) |> 
      dplyr::ungroup() |>
      dplyr::filter(keep) |>
      dplyr::select(-keep) |> 
      dplyr::distinct()

  return(ids_lookup)

}

# ...
match_participants_to_samples <- function(dataset_list) {

  # Remove pairs with a missing value. If a sample ID has many matching 
  # participant IDs across data sets (i.e., it appears many times in the 
  # lookup), then remove it from the look up.
  lookup_samp_part <- dataset_list$ids_lookup |> 
    tidyr::drop_na() |>
    dplyr::distinct() |>
    dplyr::add_count(sample_id, name ="multiplicity") |>
    dplyr::filter(multiplicity == 1) |>
    dplyr::select(-multiplicity) |>
    dplyr::rename(participant_id_lu = participant_id)

  # Add participant IDs from lookup. If a data set already has a participant
  # ID, keep non-missing values and replace missing-values with values from
  # lookup (which can themselves be missing).
  for (nm in names(dataset_list)) {

    if ("sample_id" %in% names(dataset_list[[nm]])) {

      dataset_list[[nm]] <- dataset_list[[nm]] |>
        dplyr::left_join(lookup_samp_part, by = "sample_id")

      if ("participant_id" %in% names(dataset_list[[nm]])) {

        dataset_list[[nm]] <- dataset_list[[nm]] |>
          dplyr::mutate(
            participant_id_lu = ifelse(
              is.na(participant_id),
              participant_id_lu,
              participant_id
            )
          ) |>
          dplyr::select(-participant_id)

      }

      dataset_list[[nm]] <- dataset_list[[nm]] |>
        dplyr::rename(participant_id = participant_id_lu)

    }

  }

  return(dataset_list)

}

# ... 
remove_entries_ambiguous_rna_samples <- function(dataset_list) {
  
  for (nm in names(dataset_list)) {

    ds <- dataset_list[[nm]]
    if (all(c("participant_id", "sample_id") %in% names(ds))) {
      if (any(grepl("^(zifco-11)", ds$sample_id))) {

        dataset_list[[nm]] <- ds |>
          dplyr::group_by(sample_id) |>
          dplyr::mutate(mult_samp_part = length(unique(participant_id))) |>
          dplyr::ungroup() |>
          dplyr::filter(
            !(grepl("^(zifco-11)", sample_id) & mult_samp_part > 1)
          ) |>
          dplyr::select(-mult_samp_part)

      }
    }

  }
  
  return(dataset_list)
  
}

