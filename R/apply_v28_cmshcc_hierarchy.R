# This file contains two functions: apply_v28_cms_hierarchy and set_to_zero, which is a
# helper for apply_v28_cms_hierarchy. At a high level, apply_v28_cms_hierarchy applies
# the hierarchical condition category (HCC) hierarchy defined by the Centers for Medicare
# and Medicaid Services (CMS) for version 28 of the CMS-HCC risk adjustment formula.
# set_to_zero helps by setting lower hierarchy HCC columns to zero if a higher level HCC
# is equal to 1 in a given row.

#' Helper function to set lower hierarchy columns to 0 if condition_col is 1
#'
#' This function modifies a data.table object in place
#'
#' @param curr_dt (data.table) The data.table to modify
#' @param cols (vector of strings) The columns to set to zero if condition_col is 1
#' @param condition_col (string) The column to check about being 1 or 0
#'
set_to_zero <- function(curr_dt, cols, condition_col) {
  curr_dt[get(condition_col) == 1, (cols) := 0]
}


#' Apply v28 CMS-HCC hierarchy to binary data.table
#'
#' This function applies the v28 hierachy to the input binary data.table
#'
#' @param curr_dt (data.table, also accepts data.frame or tibble) The input binary data.table, with rows corresponding to
#'   beneficiaries (with numeric row ID) and columns corresponding to HCCs (in
#'   format "hcc*")
#'
#' @return curr_dt (data.table) The input data.table with hierarchy rules applied
#' @export
apply_v28_cmshcc_hierarchy <- function(curr_dt) {
  # Convert in place to data.table if not already
  if (!data.table::is.data.table(curr_dt)) {
    data.table::setDT(curr_dt)
  }

  # Apply CMS-HCC hierarchy for each set of columns
  set_to_zero(
    curr_dt,
    c("hcc18", "hcc19", "hcc20", "hcc21", "hcc22", "hcc23"),
    "hcc17"
  )
  set_to_zero(curr_dt, c("hcc19", "hcc20", "hcc21", "hcc22", "hcc23"), "hcc18")
  set_to_zero(curr_dt, c("hcc20", "hcc21", "hcc22", "hcc23"), "hcc19")
  set_to_zero(curr_dt, c("hcc21", "hcc22", "hcc23"), "hcc20")
  set_to_zero(curr_dt, c("hcc22", "hcc23"), "hcc21")
  set_to_zero(curr_dt, c("hcc23"), "hcc22")
  set_to_zero(curr_dt, c("hcc36", "hcc37", "hcc38"), "hcc35")
  set_to_zero(curr_dt, c("hcc37", "hcc38"), "hcc36")
  set_to_zero(curr_dt, c("hcc38"), "hcc37")
  set_to_zero(curr_dt, c("hcc63", "hcc64", "hcc65", "hcc68"), "hcc62")
  set_to_zero(curr_dt, c("hcc64", "hcc65", "hcc68", "hcc202"), "hcc63")
  set_to_zero(curr_dt, c("hcc65", "hcc68"), "hcc64")
  set_to_zero(curr_dt, c("hcc78", "hcc80", "hcc81"), "hcc77")
  set_to_zero(curr_dt, c("hcc81"), "hcc80")
  set_to_zero(curr_dt, c("hcc94"), "hcc93")
  set_to_zero(curr_dt, c("hcc108"), "hcc107")
  set_to_zero(curr_dt, c("hcc112"), "hcc111")
  set_to_zero(curr_dt, c("hcc115"), "hcc114")
  set_to_zero(curr_dt, c("hcc126", "hcc127"), "hcc125")
  set_to_zero(curr_dt, c("hcc127"), "hcc126")
  set_to_zero(curr_dt, c("hcc136", "hcc137", "hcc138", "hcc139"), "hcc135")
  set_to_zero(curr_dt, c("hcc137", "hcc138", "hcc139"), "hcc136")
  set_to_zero(curr_dt, c("hcc138", "hcc139"), "hcc137")
  set_to_zero(curr_dt, c("hcc139"), "hcc138")
  set_to_zero(curr_dt, c("hcc152", "hcc153", "hcc154", "hcc155"), "hcc151")
  set_to_zero(curr_dt, c("hcc153", "hcc154", "hcc155"), "hcc152")
  set_to_zero(curr_dt, c("hcc154", "hcc155"), "hcc153")
  set_to_zero(curr_dt, c("hcc155"), "hcc154")
  set_to_zero(curr_dt, c("hcc181", "hcc182", "hcc253", "hcc254"), "hcc180")
  set_to_zero(curr_dt, c("hcc182", "hcc254"), "hcc181")
  set_to_zero(
    curr_dt,
    c("hcc180", "hcc181", "hcc182", "hcc192", "hcc253", "hcc254"),
    "hcc191"
  )
  set_to_zero(
    curr_dt,
    c("hcc180", "hcc181", "hcc182", "hcc253", "hcc254"),
    "hcc192"
  )
  set_to_zero(curr_dt, c("hcc196"), "hcc195")
  set_to_zero(curr_dt, c("hcc212", "hcc213"), "hcc211")
  set_to_zero(curr_dt, c("hcc213"), "hcc212")
  set_to_zero(
    curr_dt,
    c("hcc222", "hcc223", "hcc224", "hcc225", "hcc226", "hcc227"),
    "hcc221"
  )
  set_to_zero(
    curr_dt,
    c("hcc223", "hcc224", "hcc225", "hcc226", "hcc227"),
    "hcc222"
  )
  set_to_zero(curr_dt, c("hcc224", "hcc225", "hcc226", "hcc227"), "hcc223")
  set_to_zero(curr_dt, c("hcc225", "hcc226", "hcc227"), "hcc224")
  set_to_zero(curr_dt, c("hcc226", "hcc227"), "hcc225")
  set_to_zero(curr_dt, c("hcc227"), "hcc226")
  set_to_zero(curr_dt, c("hcc229"), "hcc228")
  set_to_zero(curr_dt, c("hcc249"), "hcc248")
  set_to_zero(curr_dt, c("hcc254"), "hcc253")
  set_to_zero(curr_dt, c("hcc264", "hcc383", "hcc409"), "hcc263")
  set_to_zero(curr_dt, c("hcc277", "hcc278", "hcc279", "hcc280"), "hcc276")
  set_to_zero(curr_dt, c("hcc278", "hcc279", "hcc280"), "hcc277")
  set_to_zero(curr_dt, c("hcc279", "hcc280"), "hcc278")
  set_to_zero(curr_dt, c("hcc280"), "hcc279")
  set_to_zero(curr_dt, c("hcc283"), "hcc282")
  set_to_zero(curr_dt, c("hcc327", "hcc328", "hcc329"), "hcc326")
  set_to_zero(curr_dt, c("hcc328", "hcc329"), "hcc327")
  set_to_zero(curr_dt, c("hcc329"), "hcc328")
  set_to_zero(curr_dt, c("hcc380", "hcc381", "hcc382", "hcc383"), "hcc379")
  set_to_zero(curr_dt, c("hcc381", "hcc382", "hcc383"), "hcc380")
  set_to_zero(curr_dt, c("hcc382", "hcc383"), "hcc381")
  set_to_zero(curr_dt, c("hcc383"), "hcc382")
  set_to_zero(curr_dt, c("hcc202", "hcc398", "hcc399"), "hcc397")
  set_to_zero(curr_dt, c("hcc202", "hcc399"), "hcc398")
  set_to_zero(curr_dt, c("hcc409"), "hcc405")
}
