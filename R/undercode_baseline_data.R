#' Undercodes (e.g. sets to zero) a specified proportion of existing diagnoses, where diagnoses are
#' selected randomly.
#'
#' @import data.table
#'
#' @param curr_dt (data.table; also accepts data.frame or tibble) The data.table to undercode
#' @param undercoding_prop (double) The proportion of all diagnoses to undercode. Must be greater
#' than zero and less than 1, exclusive.
#' @param out_dir (string) The directory to which to write output
#' @param out_file_prefix (string, Default: "undercoded_data") Prefix of output for undercoded
#'   files. Note-- by default the undercoding proportion will also be added to the file
#'   name as a suffix (e.g. undercoding_prop = 0.25 will be represented as "0pt25")
#' @param curr_seed (int, Default: 123) Seed for reproducibility
#' @export
undercode_dt <- function(
    curr_dt,
    undercoding_prop,
    out_dir,
    out_file_prefix = "undercoded_data",
    curr_seed = 123) {
  withr::local_seed(curr_seed)

  # Convert in place to data.table if not already
  if (!data.table::is.data.table(curr_dt)) {
    data.table::setDT(curr_dt)
  }

  # Check curr_dt is formatted correctly
  check_curr_dt(curr_dt)

  # Check if out_dir exists and make if needed
  if (!dir.exists(here(out_dir))) {
    dir.create(here(out_dir), recursive = TRUE) # Also make parent directories if needed
  }

  # Identify indexes to potentially undercode
  coded_indexes <- which(curr_dt == 1, arr.ind = TRUE)
  coded_indexes <- coded_indexes[coded_indexes[, "col"] != 1, ] # remove values from person_id column

  # Randomly sample a proportion of identified indexes
  # Where proportion == undercoding_prop
  undercoding_index_rows <- sample(
    seq_len(nrow(coded_indexes)),
    floor(undercoding_prop * nrow(coded_indexes))
  )
  indexes_to_undercode <- coded_indexes[undercoding_index_rows, ]

  if (is.matrix(indexes_to_undercode)) {
    # Convert to data.table for more efficient grouping
    indexes_dt <- data.table(
      row = indexes_to_undercode[, "row"],
      col = indexes_to_undercode[, "col"]
    )

    indexes_dt[,
      {
        data.table::set(curr_dt, i = row, j = col, value = 0)
        NULL
      },
      by = col
    ]
  } else {
    # edge case where there is only one index to undercode
    data.table::set(
      curr_dt,
      i = indexes_to_undercode[1],
      j = indexes_to_undercode[2],
      value = 0
    )
  }

  # Write to file
  out_name <- paste(out_file_prefix, undercoding_prop, sep = "_")
  out_name <- gsub("\\.", "pt", out_name) # to avoid periods in file name
  out_path <- fs::path(here::here(), out_dir, paste0(out_name, ".csv"))

  data.table::fwrite(curr_dt, out_path)
}
