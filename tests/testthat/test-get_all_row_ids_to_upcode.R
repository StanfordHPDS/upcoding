test_that("get_all_row_ids_to_upcode() works for one hcc", {
  # Example valid spec df
  hcc <- c("hcc35")
  approach <- c("lower severity") # What approach to use to select
  upcoding_prop <- c(0.75) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  # Make mini dt
  mini_dt <- data.table(matrix(0, 4, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:4)

  mini_dt[c(1, 4), "hcc37" := 1]
  mini_dt[2, "hcc36" := 1]
  mini_dt[3, "hcc38" := 1]

  # only one timepoint
  rid_tbl <- get_all_row_ids_to_upcode(my_upcoding_spec_df, mini_dt, 1)

  # Should correspond to single HCC
  expect_equal(nrow(rid_tbl), 1)

  # 0.75*4 = 3 rows should be upcoded
  expect_length(rid_tbl$row_ids[[1]], 3)
})

test_that("get_all_row_ids_to_upcode() works for multiple hccs", {
  # Example valid spec df
  hcc <- c("hcc35", "hcc2")
  approach <- c("lower severity", "any") # What approach to use to select
  upcoding_prop <- c(0.75, 0.5) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  # Make mini dt
  mini_dt <- data.table(matrix(0, 4, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:4)

  mini_dt[c(1, 4), "hcc37" := 1]
  mini_dt[2, "hcc36" := 1]
  mini_dt[3, "hcc38" := 1]

  # only one timepoint
  rid_tbl <- get_all_row_ids_to_upcode(my_upcoding_spec_df, mini_dt, 1)

  # Should correspond to two HCCs
  expect_equal(nrow(rid_tbl), 2)
  expect_equal(rid_tbl$hcc, my_upcoding_spec_df$hcc)

  # 0.75*4 = 3 rows should be upcoded
  expect_length(rid_tbl$row_ids[[1]], 3)

  # 0.5*4 = 2 rows should be upcoded
  expect_length(rid_tbl$row_ids[[2]], 2)
})

test_that("get_all_row_ids_to_upcode() works for multiple time points", {
  # Example valid spec df
  hcc <- c("hcc2")
  approach <- c("any") # What approach to use to select
  upcoding_prop <- c(0.8) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  # Make mini dt
  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  rid_tbl <- get_all_row_ids_to_upcode(my_upcoding_spec_df, mini_dt, 3)

  # Should correspond to three time points
  expect_equal(nrow(rid_tbl), 3)

  # count of rids by timepoint should add up to count of total rids
  expect_equal(
    sum(
      length(rid_tbl$row_ids[[1]]),
      length(rid_tbl$row_ids[[2]]),
      length(rid_tbl$row_ids[[3]])
    ),
    8
  )
})

test_that("get_row_ids_to_upcode_for_single_hcc() works with 'any' option", {
  mini_dt <- data.table(matrix(0, 4, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- c(1, 2, 3, 4)

  mini_dt[1, "hcc35" := 1]
  mini_dt[c(1, 2), "hcc37" := 1]
  mini_dt[4, "hcc36" := 1]
  mini_dt[4, "hcc38" := 1]

  curr_hcc <- "hcc37"
  curr_approach <- "any"
  curr_upcoding_prop <- 0.5

  expect_length(
    get_row_ids_to_upcode_for_single_hcc(
      curr_hcc,
      curr_approach,
      curr_upcoding_prop,
      mini_dt
    ),
    1
  )
})

test_that("get_row_ids_to_upcode_for_single_hcc() works with lower severity option", {
  mini_dt <- data.table(matrix(0, 4, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- c(1, 2, 3, 4)

  mini_dt[c(1, 2, 3), "hcc37" := 1]
  mini_dt[4, "hcc36" := 1]
  mini_dt[4, "hcc38" := 1]

  curr_hcc <- "hcc35"
  curr_approach <- "lower severity"
  curr_upcoding_prop <- 0.5

  expect_length(
    get_row_ids_to_upcode_for_single_hcc(
      curr_hcc,
      curr_approach,
      curr_upcoding_prop,
      mini_dt
    ),
    2
  )
})
