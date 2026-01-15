#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom memoise memoise
#' @importFrom stats rmultinom
#' @importFrom stats setNames
## usethis namespace: end
NULL

# Suppress R CMD check notes about variables used in data.table and other contexts
utils::globalVariables(c(
  ".",
  "approach",
  "cms_hcc_v28_reference_hcc_count_dt",
  "cooccurring_v28_hcc_counts_allofus",
  "event_type",
  "hcc",
  "hcc_hierarchy_list",
  "hcc_set",
  "hccs",
  "included",
  "name",
  "num_respondents",
  "person_id",
  "row_id_splits",
  "row_ids",
  "set_index",
  "timepoint",
  "upcoding_prop",
  "v28_hccs",
  "value"
))
