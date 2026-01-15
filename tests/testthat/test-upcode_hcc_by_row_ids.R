test_that("upcode_hcc_by_row_ids() works as expected", {
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

  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  all_row_ids_to_upcode_tbl <- get_all_row_ids_to_upcode(
    my_upcoding_spec_df,
    mini_dt,
    2
  )

  curr_hcc <- "hcc2"
  curr_t <- 2

  upcode_hcc_by_row_ids(
    mini_dt,
    all_row_ids_to_upcode_tbl,
    curr_hcc,
    curr_t
  )

  expect_equal(sum(mini_dt[[curr_hcc]]), 4)
})
