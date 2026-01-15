# This file contains the function get_all_row_ids_to_censor, which gets specifics
# row ids to censor across the number of time points specified. This includes several
# helper functions:
#
# - get_initial_row_ids_to_censor: samples a proportion of all rows to censor over all time points
# - aggregate_censoring_rids_cumulatively: Aggregates censored rows cumulatively by time point
# - format_censoring_row_ids_to_tibble: Formats censored row IDs into a tibble for easier lookup later

#' Get overall list of row IDs to censor for an individual HCC
#'
#' This samples (without replacement) a list of row indexes to censor from the
#' overall dataset, so that the user-specified upcoding proportion and approach
#' occurs.
#'
#' @param curr_dt (data.table) Binary data.table with rows corresponding to people and
#'   columns corresponding to 115 HCCs (from v28 of CMS-HCC) and one column for
#'   person_ids.
#' @param num_timepoints (int) The number of timepoints across which to censor sets of beneficiaries
#' @param censoring_prop (double) The proportion of beneficiaries to
#'   censor overall (across all timepoints). Must be greater than 0 and less than 1, exclusive.
#'
#' @return A tibble of row IDs to censor at each time point
#'
#' @export
get_all_row_ids_to_censor <- function(curr_dt, num_timepoints, censoring_prop) {
  # identify which rows to censor overall
  rids_vec_to_censor <- get_initial_row_ids_to_censor(curr_dt, censoring_prop)

  if (length(rids_vec_to_censor) > 0) {
    # split censored row ids by number of time points
    rids_to_censor_by_timepoint <- split_row_ids_into_timepoints(
      rids_vec_to_censor,
      num_timepoints
    )

    # aggregate censored row ids cumulatively
    cumulative_rids_to_censor_by_timepoint <- aggregate_censoring_rids_cumulatively(
      rids_to_censor_by_timepoint
    )

    # format as tibble for easier lookup
    rids_to_censor_by_timepoint_tbl <- format_censoring_row_ids_to_tibble(
      cumulative_rids_to_censor_by_timepoint
    )

    rids_to_censor_by_timepoint_tbl
  } else {
    NULL
  }
}

get_initial_row_ids_to_censor <- function(curr_dt, censoring_prop) {
  all_row_ids_to_censor <- curr_dt |>
    dplyr::slice_sample(prop = censoring_prop) |>
    dplyr::pull(person_id)
}

aggregate_censoring_rids_cumulatively <- function(rids_vec_to_censor) {
  cumulative_rids <- c()
  cumulative_rids_list <- list()

  # aggregate cumulatively
  for (i in seq_along(rids_vec_to_censor)) {
    cumulative_rids <- c(cumulative_rids, rids_vec_to_censor[[i]])
    cumulative_rids_list[[i]] <- unique(cumulative_rids)
  }
  cumulative_rids_list
}

format_censoring_row_ids_to_tibble <- function(censored_rids_by_timepoint) {
  rids_to_censor_tbl <- censored_rids_by_timepoint |>
    tibble::enframe() |>
    dplyr::rename(timepoint = name, row_ids = value)
}
