# Overall, this file includes the function simulate_baseline_v28_hcc_dt as well as a
# helper function. See each function's documentation for details.

#' Simulate baseline HCC values using reference hcc sets and CMS-HCC v28
#' hierarchy
#'
#' This function simulates a binary data.table of 115 HCCs corresponding to
#' those used in version 28 (v28) of the CMS-HCC risk adjustment payment model.
#' It samples from a reference dataset of co-occurring HCCs sets with counts (e.g. weights);
#' see cms_hcc_v28_reference_hcc_count_dt (the default reference dataset) or the documentation of
#' make_reference_dataset for more details about this. If specified, the function
#' also applies the CMS-HCC v28 hierarchy to columns by row.
#'
#' @param num_people (int) The number of people (e.g. rows) to simulate, represented
#'   as an integer.
#' @param out_dir (string) Directory to write files
#' @param reference_cooccurring_hcc_count (data.table, also accepts dataframe or tibble Default: `cms_hcc_v28_reference_hcc_count_dt`)
#' @param apply_v28_hierarchy (boolean, Default: FALSE) Whether to apply the v28 hierarchy to the data after upcoding
#' @param out_file_prefix (string, default: `"baseline_data"`) Prefix used for output
#'   files. The dataset will be written as `[out_file_prefix].csv`, and timestamps
#'   as `[out_file_prefix]_times.csv`.
#' @param curr_seed (int, Default: 123) Seed for reproducibility
#'
#' @export
simulate_baseline_v28_hcc_dt <- function(
    num_people,
    out_dir,
    reference_cooccurring_hcc_count = cms_hcc_v28_reference_hcc_count_dt,
    apply_v28_hierarchy = FALSE,
    out_file_prefix = "baseline_data",
    curr_seed = 123) {
  withr::local_seed(curr_seed)

  # Check if out_dir exists and make if needed
  if (!dir.exists(here::here(out_dir))) {
    # Also make parent directories if needed
    dir.create(here::here(out_dir), recursive = TRUE)
  }

  # Get pre-processed reference data (automatically cached by memoise)
  ref_cache <- preprocess_reference_data(reference_cooccurring_hcc_count)

  # Sample indices
  sampled_indices <- sample.int(
    length(ref_cache$index_sets),
    size = num_people,
    replace = TRUE,
    prob = ref_cache$probs
  )

  # Pre-allocate result matrix
  result_matrix <- matrix(0L, nrow = num_people, ncol = length(v28_hccs))

  # Fill matrix using pre-computed indices
  for (i in seq_len(num_people)) {
    col_indices <- ref_cache$index_sets[[sampled_indices[i]]]
    if (length(col_indices) > 0) {
      result_matrix[i, col_indices] <- 1L
    }
  }

  # Convert to data.table
  curr_hcc_dt <- data.table::data.table(
    person_id = seq_len(num_people),
    result_matrix
  )
  data.table::setnames(curr_hcc_dt, c("person_id", v28_hccs))

  # Apply CMS-HCC v28 hierarchy if specified
  if (apply_v28_hierarchy) {
    curr_hcc_dt <- apply_v28_cmshcc_hierarchy(curr_hcc_dt)
  }

  # Write baseline data to file
  data.table::fwrite(
    curr_hcc_dt,
    fs::path(here::here(), out_dir, paste0(out_file_prefix, ".csv"))
  )
}

# Create memoised version of the preprocessing function
# This caches the pre-processed reference data for performance
preprocess_reference_data <- memoise::memoise(function(
    reference_cooccurring_hcc_count) {
  hcc_indices <- setNames(seq_along(v28_hccs), v28_hccs)

  # Pre-split and convert to indices
  split_sets <- strsplit(
    reference_cooccurring_hcc_count$hcc_set,
    ", ",
    fixed = TRUE
  )
  index_sets <- lapply(split_sets, function(hccs) {
    if (length(hccs) > 0 && hccs[1] != "") {
      idx <- hcc_indices[hccs]
      idx[!is.na(idx)]
    } else {
      integer(0)
    }
  })

  list(
    index_sets = index_sets,
    probs = reference_cooccurring_hcc_count$num_respondents
  )
})
