test_that("Error message about columns works as expected", {
  mini_dt <- data.table(matrix(0, 4, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- c(1, 2, 3, 4) # set ids

  other_dt1 <- copy(mini_dt)
  other_dt2 <- copy(mini_dt)

  # remove a couple columns from one copy
  other_dt1[, c("hcc401", "hcc202") := NULL]
  expect_error(check_curr_dt(other_dt1))

  # set columns to uppercase
  colnames(other_dt2) <- toupper(colnames(other_dt2))
  expect_no_error(check_curr_dt(other_dt2))
})

test_that("format_as_data_table() works as expected", {
  # Example valid data.table
  mini_dt <- data.table(matrix(0, 4, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- c(1, 2, 3, 4) # set ids

  expect_no_error(format_as_data_table(mini_dt))

  expect_error(format_as_data_table("my_curr_dt"))
})
