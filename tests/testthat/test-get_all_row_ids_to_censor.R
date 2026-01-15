test_that("get_initial_row_ids_to_censor() returns expected number of rows", {
  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  mini_dt[1, "hcc35" := 1]
  mini_dt[c(1, 2), "hcc37" := 1]
  mini_dt[4, "hcc36" := 1]
  mini_dt[4, "hcc38" := 1]

  curr_censoring_prop <- 0.5

  # identify which rows to censor overall
  # should be 0.5*10 = 5 row ids
  rids_vec_to_censor <- get_initial_row_ids_to_censor(
    mini_dt,
    curr_censoring_prop
  )
  expect_length(rids_vec_to_censor, 5)
})

test_that("get_all_row_ids_to_censor() works as expected in standard case", {
  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  mini_dt[1, "hcc35" := 1]
  mini_dt[c(1, 2), "hcc37" := 1]
  mini_dt[4, "hcc36" := 1]
  mini_dt[4, "hcc38" := 1]

  curr_censoring_prop <- 0.5
  curr_num_timepoints <- 2

  rids_to_censor_tbl <- get_all_row_ids_to_censor(
    mini_dt,
    curr_num_timepoints,
    curr_censoring_prop
  )

  # Should have two rows (for two time points)
  expect_equal(nrow(rids_to_censor_tbl), 2)

  # Should cumulatively have five row ids in total
  expect_equal(length(rids_to_censor_tbl$row_ids[[curr_num_timepoints]]), 5)
})

test_that("get_all_row_ids_to_censor() works when there are no rows to censor", {
  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  mini_dt[1, "hcc35" := 1]
  mini_dt[c(1, 2), "hcc37" := 1]
  mini_dt[4, "hcc36" := 1]
  mini_dt[4, "hcc38" := 1]

  curr_censoring_prop <- 0.05
  curr_num_timepoints <- 2

  rids_to_censor_tbl <- get_all_row_ids_to_censor(
    mini_dt,
    curr_num_timepoints,
    curr_censoring_prop
  )

  expect_null(rids_to_censor_tbl)
})
