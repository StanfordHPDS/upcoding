test_that("undercode_baseline_data() works as expected with more than one row", {
  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs) # Note. Not setting person_ids to actual values here
  expect_equal(sum(mini_dt), 0) # check there are no coded diagnoses

  # Set 10 values to be coded
  mini_dt[3, c("hcc191", "hcc277", "hcc329") := 1]
  mini_dt[4, c("hcc6") := 1]
  mini_dt[9, c("hcc19", "hcc20", "hcc21", "hcc22", "hcc463") := 1]
  mini_dt[10, c("hcc20") := 1]
  expect_equal(sum(mini_dt), 10) # check there are 10 coded diagnoses

  # undercode then read in resulting file
  undercode_dt(
    mini_dt,
    0.2, # should correspond to two rows
    out_dir = "temp"
  )

  # read in generated data
  undercoded_dt <- readr::read_csv(
    here("temp/undercoded_data_0pt2.csv"),
    show_col_types = FALSE
  )
  # remove temporary directory and files
  on.exit(unlink(here("temp/"), recursive = TRUE), add = TRUE)

  # verify undercoding removed expected number of diagnoses
  expect_equal(sum(undercoded_dt), 8)
})

test_that("undercode_baseline_data() works as expected with only one row", {
  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs) # Note. Not setting person_ids to actual values here
  expect_equal(sum(mini_dt), 0) # check there are no coded diagnoses

  # Set 10 values to be coded
  mini_dt[3, c("hcc191", "hcc277", "hcc329") := 1]
  mini_dt[4, c("hcc6") := 1]
  mini_dt[9, c("hcc19", "hcc20", "hcc21", "hcc22", "hcc463") := 1]
  mini_dt[10, c("hcc20") := 1]
  expect_equal(sum(mini_dt), 10) # check there are 10 coded diagnoses

  # undercode then read in resulting file
  undercode_dt(
    mini_dt,
    0.1, # should correspond to 1 row
    out_dir = "temp"
  )

  # read in generated data
  undercoded_dt <- readr::read_csv(
    here("temp/undercoded_data_0pt1.csv"),
    show_col_types = FALSE
  )
  # remove temporary directory and files
  on.exit(unlink(here("temp/"), recursive = TRUE), add = TRUE)

  # verify undercoding removed expected number of diagnoses
  expect_equal(sum(undercoded_dt), 9)
})
