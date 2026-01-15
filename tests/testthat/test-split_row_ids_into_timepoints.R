test_that("split_row_ids_into_timepoints() returns error for empty row id vectors", {
  empty_row_ids <- c()
  expect_error(split_row_ids_into_timepoints(
    empty_row_ids,
    4
  ))
})

test_that("split_row_ids_into_timepoints() row ids into expected number of rows", {
  curr_row_ids <- seq(1, 20)

  expect_equal(length(split_row_ids_into_timepoints(curr_row_ids, 4)), 4)
})
