#' Splits the overall row IDs to upcode into multinomially distributed random
#' group sizes, where the number of groups is specified by the user-supplied
#' number of timepoints.
#'
#' This splits the overall set of row IDs to upcode into num_timepoints number
#' of groups. The group sizes are multinomially distributed with equal
#' probability per group, but this can be varied by the user if you prefer
#' alternate groupings.
#'
#' @param row_ids (vector or list) One set of row IDs to split into groups. This should be the
#'   output of get_all_row_ids_to_upcode.
#' @param num_timepoints (int) The number of timepoints to use.
#'
#' @return A list of length num_timepoints with the row_ids randomly distributed across each element
#'
#' @export
split_row_ids_into_timepoints <- function(row_ids, num_timepoints) {
  if (length(row_ids) == 0) {
    stop("At least one row id must be specified")
  }

  # if number of row ids is less than the number of timepoints,
  # make the number of timepoints correspond to length of row_ids
  if (length(row_ids) < num_timepoints) {
    num_timepoints <- length(row_ids)
  }

  # Get group sizes
  group_sizes <- rmultinom(
    n = 1,
    size = length(row_ids),
    prob = rep(1 / num_timepoints, num_timepoints)
  )[, 1]

  # split the random row IDs based on group sizes
  row_ids_by_group <- vctrs::vec_chop(row_ids, sizes = group_sizes)

  row_ids_by_group
}
