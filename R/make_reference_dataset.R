# Overall, this file includes the function make_reference_dataset as well as four small
# helper functions. See each function's documentation for details.

#' Make reference data set
#'
#' This is an optional helper function for simulate_baseline_v28_hcc_dt, in case users
#' don't want to use the default value for reference_cooccurring_hcc_count (which is
#' cms_hcc_v28_reference_hcc_count_dt).
#'
#' @param coocurring_hcc_counts (tibble, Default: `cooccurring_v28_hcc_counts_allofus`). What
#' reference set of co-occurring HCC sets to use for building the reference data. This tibble
#' is expected to have two columns (`hcc_set` and `num_respondents`) and (after inserting
#' missing HCCs, if needed) will be used to sample rows for the baseline data. `hcc_set`
#' corresponds to co-occurring HCCs to code as being present in baseline data, and `num_respondents`
#' acts as a weight for sampling (so HCC sets with higher `num_respondent` values will be sampled
#' more in creating baseline data).
#' @param missing_hcc_respondent_count (int, Default: 21) The respondent count to use for HCCs
#' that are missing from co-occurring_hcc_counts. By default this value is 21, consistent with
#' AllOfUs' minimum value requirement for data dissemination.
#' @param randomly_insert_missing_hccs_into_cooccurring_sets (boolean, Default: TRUE). Whether
#' to randomly insert HCCs not included in cooccurring_hcc_counts into existing HCC sets or not. The
#' reason a user might want to do this is to not disproportionately penalize missing HCCs in
#' baseline sampling in the future (assuming there are not many missing HCCs relative to the
#' overall number of co-occurring HCC sets).
#' @param missing_hcc_insertion_proportion (double, Default: 0.25) What proportion of the time
#' to insert HCCs not included in cooccurring_hcc_counts into existing HCC sets when building the
#' reference dataset to sample from. Must be greater than 0 and less than 1, exclusive. Each
#' HCC is inserted separately into missing_hcc_insertion_proportion times the number of existing reference sets.
#' @param curr_seed (int, Default: 123) Seed for reproducibility
#'
#' @export
make_reference_dataset <- function(
    coocurring_hcc_counts = cooccurring_v28_hcc_counts_allofus,
    missing_hcc_respondent_count = 21,
    randomly_insert_missing_hccs_into_cooccurring_sets = TRUE,
    missing_hcc_insertion_proportion = 0.25,
    curr_seed = 123) {
  withr::local_seed(curr_seed)

  # Validate proportion parameter
  if (
    missing_hcc_insertion_proportion < 0 || missing_hcc_insertion_proportion > 1
  ) {
    stop(
      "missing_hcc_insertion_proportion must be between 0 and 1, got: ",
      missing_hcc_insertion_proportion
    )
  }
  # Identify which HCCs are present in reference data
  present_hccs <- coocurring_hcc_counts |>
    tidyr::separate_longer_delim(hcc_set, ", ") |>
    dplyr::rowwise() |>
    dplyr::pull(hcc_set) |>
    unique()

  # Identify which HCCs are missing compared with the version 28 model.
  # Note. v28 HCCs are loaded as v28_hccs in the package if you want to see
  # what they are.
  missing_hccs <- v28_hccs[!v28_hccs %in% present_hccs]

  # Set counts for missing HCCs to missing_hcc_respondent count
  missing_hcc_dt <- data.table(
    hcc_set = missing_hccs,
    num_respondents = rep(
      missing_hcc_respondent_count,
      length(missing_hccs)
    )
  )

  # Since missing HCCs will be penalized disproportionately if they are left as
  # individual HCCs, we also randomly add each missing HCC to a proportion of
  # existing HCC sets (if user specified):
  if (randomly_insert_missing_hccs_into_cooccurring_sets) {
    num_sets <- round(
      nrow(coocurring_hcc_counts) * missing_hcc_insertion_proportion
    )

    if (length(missing_hccs) > 0 && num_sets > 0) {
      coocurring_hcc_counts <- data.table::as.data.table(coocurring_hcc_counts)

      set_hcc_combinations <- lapply(missing_hccs, function(h) {
        set_indexes <- sample(
          seq_len(nrow(coocurring_hcc_counts)),
          num_sets,
          replace = FALSE
        )
        data.table::data.table(set_index = set_indexes, hcc = h)
      })

      all_combinations <- data.table::rbindlist(set_hcc_combinations)
      data.table::setkey(all_combinations, set_index)
      hccs_to_add <- all_combinations[,
        .(hccs = paste(hcc, collapse = ", ")),
        by = set_index
      ]

      coocurring_hcc_counts[, set_index := .I]
      coocurring_hcc_counts[
        hccs_to_add,
        hcc_set := paste(hcc_set, hccs, sep = ", "),
        on = .(set_index)
      ]
      coocurring_hcc_counts[, set_index := NULL]
    }
  }

  # Combine real-world and imputed HCCs into a reference data.table
  reference_hcc_dt <- rbind(coocurring_hcc_counts, missing_hcc_dt) |>
    dplyr::mutate(
      num_respondents = round(as.numeric(num_respondents))
    ) |>
    data.table::as.data.table()

  return(reference_hcc_dt)
}
