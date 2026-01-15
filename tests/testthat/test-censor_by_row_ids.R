test_that("censor_hcc_by_row_ids() works as expected", {
  withr::local_seed(123)

  # Example valid spec df
  hcc <- c("hcc35", "hcc2")
  approach <- c("any", "any") # What approach to use to select
  upcoding_prop <- c(0.75, 0.5) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  mini_dt <- data.table(matrix(1, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)
  initial_sum <- sum(mini_dt[, .SD, .SDcols = !c("person_id")]) # to compare later

  num_timepoints <- 2
  censoring_prop <- 0.5
  all_row_ids_to_censor_tbl <- get_all_row_ids_to_censor(
    mini_dt,
    num_timepoints,
    censoring_prop
  )

  curr_t <- 2
  censor_by_row_ids(
    mini_dt,
    all_row_ids_to_censor_tbl,
    curr_t
  )

  # expected number of 1's is num(v28_hccs)*5 = 115*5 = 575
  count_of_1s <- sum(mini_dt[, .SD, .SDcols = !c("person_id")])
  expect_equal(count_of_1s, 575)
})
