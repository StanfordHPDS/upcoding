# At a high level, this file has a function check_upcoding_spec_df as well as its helpers
# (check_upcoding_spec_df_class, check_upcoding_spec_df_colnames, check_upcoding_spec_df_hccs,
# check_upcoding_spec_df_approach, check_upcoding_spec_df_props). check_upcoding_spec_df_class
# verifies that upcoding_spec_df is a tibble or data.frame, and the other helpers verify individual
# columns of upcoding_spec_df are formatted as expected.

#' Checks whether upcoding specification dataframes are formatted correctly
#'
#' Verifies whether upcoding_spec_df is a tibble or data frame,column names,
#' whether hccs are valid v28 hccs, whether approaches are one of valid options,
#' and if upcoding_proportions are between 0 and 1.
#'
#' @param upcoding_spec_df (tibble or dataframe) Specifies which HCCs to upcode and how. This
#'   is expected to have columns 'hcc' (containing any of the v28 HCCs), 'approach'
#'   (either 'any' or 'lower severity' are allowed), and 'upcoding_prop' (must be a proportion
#'   greater than 0 and less than 1)
#' @export
check_upcoding_spec_df <- function(upcoding_spec_df) {
  check_upcoding_spec_df_class(upcoding_spec_df)

  check_upcoding_spec_df_colnames(upcoding_spec_df)

  check_upcoding_spec_df_hccs(upcoding_spec_df)

  check_upcoding_spec_df_approach(upcoding_spec_df)

  check_upcoding_spec_df_props(upcoding_spec_df)
}

check_upcoding_spec_df_class <- function(upcoding_spec_df) {
  if (!tibble::is_tibble(upcoding_spec_df) & !is.data.frame(upcoding_spec_df)) {
    stop("upcoding_spec_df must be a data.frame or a tibble")
  }
}

check_upcoding_spec_df_colnames <- function(upcoding_spec_df) {
  # set column names to lower case
  colnames(upcoding_spec_df) <- tolower(colnames(upcoding_spec_df))

  # check column names
  required_cols <- c("hcc", "approach", "upcoding_prop")
  missing_cols <- setdiff(required_cols, colnames(upcoding_spec_df))

  if (length(missing_cols) > 0) {
    stop(
      paste(missing_cols, collapse = ", "),
      "must be column(s) in upcoding_spec_df"
    )
  }
}

check_upcoding_spec_df_hccs <- function(upcoding_spec_df) {
  # set hccs to lower case
  upcoding_spec_df$hcc <- tolower(upcoding_spec_df$hcc)

  # identify any that are not a valid v28 HCC
  invalid_hccs <- setdiff(upcoding_spec_df$hcc, v28_hccs)
  if (length(invalid_hccs) > 0) {
    stop(paste(paste(invalid_hccs, collapse = ", "), "are not valid v28 HCCs"))
  }
}

check_upcoding_spec_df_approach <- function(upcoding_spec_df) {
  # set to lower case
  upcoding_spec_df$approach <- tolower(upcoding_spec_df$approach)

  # check if any are invalid approaches (must be either 'any' or 'lower severity')
  invalid_approaches <- setdiff(
    upcoding_spec_df$approach,
    c("any", "lower severity")
  )
  if (length(invalid_approaches) > 0) {
    stop(paste(
      paste(invalid_approaches, collapse = ", "),
      "are not valid approaches; must be either 'any' or 'lower severity'"
    ))
  }
}

check_upcoding_spec_df_props <- function(upcoding_spec_df) {
  if (!is.numeric(upcoding_spec_df$upcoding_prop)) {
    stop("upcoding_prop must be a numeric column")
  }

  if (length(setdiff(upcoding_spec_df$upcoding_prop > 0, TRUE)) > 0) {
    stop("All upcoding_prop values must be greater than 0")
  } else if (length(setdiff(upcoding_spec_df$upcoding_prop < 1, TRUE)) > 0) {
    stop("All upcoding_prop values must be less than 1")
  }
}
