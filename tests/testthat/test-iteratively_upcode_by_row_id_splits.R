test_that("iteratively_upcode_by_row_id_splits() works as expected without v28 hierarchy option", {
  withr::local_seed(123)

  hcc <- c("hcc35", "hcc2")
  approach <- c("any", "any") # What approach to use to select
  upcoding_prop <- c(0.5, 0.5) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  curr_censoring_prop <- 0.5
  curr_num_timepoints <- 2
  all_row_ids_to_upcode_tbl <- get_all_row_ids_to_upcode(
    my_upcoding_spec_df,
    mini_dt,
    2
  )
  all_row_ids_to_censor_tbl <- get_all_row_ids_to_censor(
    mini_dt,
    curr_num_timepoints,
    curr_censoring_prop
  )

  curr_hcc <- "hcc2"
  # Get row IDs we expect to be upcoded by timepoint
  # Time 1
  upcoded_ids_time1 <- all_row_ids_to_upcode_tbl |>
    dplyr::filter(hcc == curr_hcc & timepoint == 1) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids)
  censored_ids_time1 <- all_row_ids_to_censor_tbl |>
    dplyr::filter(timepoint == 1) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids) |>
    unique()
  remaining_upcoded_ids_time1 <- upcoded_ids_time1[
    !upcoded_ids_time1 %in% censored_ids_time1
  ]
  # Time 2
  upcoded_ids_time2 <- all_row_ids_to_upcode_tbl |>
    dplyr::filter(hcc == curr_hcc & timepoint == 2) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids)
  censored_ids_time2 <- all_row_ids_to_censor_tbl |>
    dplyr::filter(timepoint == 2) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids) |>
    unique()
  remaining_upcoded_ids_time2 <- upcoded_ids_time2[
    !upcoded_ids_time2 %in% censored_ids_time2
  ]

  iteratively_upcode_by_row_id_splits(
    mini_dt,
    all_row_ids_to_upcode_tbl,
    all_row_ids_to_censor_tbl,
    curr_hcc,
    "temp",
    "temp_upcoded",
    FALSE
  )

  # read in generated data
  upcoded_events <- readr::read_csv(
    here::here("temp/hcc2_temp_upcoded_event_and_time_labels.csv"),
    show_col_types = FALSE
  )

  # remove temporary directory and files
  on.exit(unlink(here("temp/"), recursive = TRUE), add = TRUE)

  # Get recorded events by timepoint
  recorded_upcoding_events_time1 <- upcoded_events |>
    dplyr::filter(event_type == 1 & event_time == 1) |>
    dplyr::pull(person_id)
  recorded_upcoding_events_time2 <- upcoded_events |>
    dplyr::filter(event_type == 1 & event_time == 2) |>
    dplyr::pull(person_id)

  # check expected row IDs were upcoded
  expect_equal(
    sort(recorded_upcoding_events_time1),
    sort(remaining_upcoded_ids_time1)
  )
  expect_equal(
    sort(recorded_upcoding_events_time2),
    sort(remaining_upcoded_ids_time2)
  )
})

test_that("iteratively_upcode_by_row_id_splits() works as expected without v28 hierarchy option - more rows", {
  withr::local_seed(123)

  hcc <- c("hcc35", "hcc2")
  approach <- c("any", "any") # What approach to use to select
  upcoding_prop <- c(0.5, 0.5) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  mini_dt <- data.table(matrix(0, 100, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:100)

  curr_censoring_prop <- 0.05
  curr_num_timepoints <- 4
  all_row_ids_to_upcode_tbl <- get_all_row_ids_to_upcode(
    my_upcoding_spec_df,
    mini_dt,
    2
  )
  all_row_ids_to_censor_tbl <- get_all_row_ids_to_censor(
    mini_dt,
    curr_num_timepoints,
    curr_censoring_prop
  )

  curr_hcc <- "hcc2"
  # Get row IDs we expect to be upcoded by timepoint
  # Time 1
  upcoded_ids_time1 <- all_row_ids_to_upcode_tbl |>
    dplyr::filter(hcc == curr_hcc & timepoint == 1) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids)
  censored_ids_time1 <- all_row_ids_to_censor_tbl |>
    dplyr::filter(timepoint == 1) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids) |>
    unique()
  remaining_upcoded_ids_time1 <- upcoded_ids_time1[
    !upcoded_ids_time1 %in% censored_ids_time1
  ]
  # Time 2
  upcoded_ids_time2 <- all_row_ids_to_upcode_tbl |>
    dplyr::filter(hcc == curr_hcc & timepoint == 2) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids)
  censored_ids_time2 <- all_row_ids_to_censor_tbl |>
    dplyr::filter(timepoint == 2) |>
    tidyr::unnest(row_ids) |>
    dplyr::pull(row_ids) |>
    unique()
  remaining_upcoded_ids_time2 <- upcoded_ids_time2[
    !upcoded_ids_time2 %in% censored_ids_time2
  ]

  iteratively_upcode_by_row_id_splits(
    mini_dt,
    all_row_ids_to_upcode_tbl,
    all_row_ids_to_censor_tbl,
    curr_hcc,
    "temp",
    "temp_upcoded",
    FALSE
  )

  # read in generated data
  upcoded_events_label <- readr::read_csv(
    here::here("temp/hcc2_temp_upcoded_event_and_time_labels.csv"),
    show_col_types = FALSE
  )

  # remove temporary directory and files
  on.exit(unlink(here("temp/"), recursive = TRUE), add = TRUE)

  # Get recorded events by timepoint
  recorded_upcoding_events_time1 <- upcoded_events_label |>
    dplyr::filter(event_type == 1 & event_time == 1) |>
    dplyr::pull(person_id)
  recorded_upcoding_events_time2 <- upcoded_events_label |>
    dplyr::filter(event_type == 1 & event_time == 2) |>
    dplyr::pull(person_id)

  # check expected row IDs were upcoded
  expect_equal(
    sort(recorded_upcoding_events_time1),
    sort(remaining_upcoded_ids_time1)
  )
  expect_equal(
    sort(recorded_upcoding_events_time2),
    sort(remaining_upcoded_ids_time2)
  )
})


test_that("iteratively_upcode_by_row_id_splits() works as expected with v28 hierarchy option", {
  withr::local_seed(123)

  hcc <- c("hcc36")
  approach <- c("any") # What approach to use to select
  upcoding_prop <- c(0.5) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  mini_dt[seq(1:10), "hcc35" := 1]

  curr_censoring_prop <- 0.1
  curr_num_timepoints <- 2
  all_row_ids_to_upcode_tbl <- get_all_row_ids_to_upcode(
    my_upcoding_spec_df,
    mini_dt,
    2
  )
  all_row_ids_to_censor_tbl <- get_all_row_ids_to_censor(
    mini_dt,
    curr_num_timepoints,
    curr_censoring_prop
  )

  curr_hcc <- "hcc36"
  iteratively_upcode_by_row_id_splits(
    mini_dt,
    all_row_ids_to_upcode_tbl,
    all_row_ids_to_censor_tbl,
    curr_hcc,
    "temp",
    "temp_upcoded",
    apply_v28_hierarchy = TRUE
  )

  # read in generated data
  upcoded_events_hcc36 <- readr::read_csv(
    here::here("temp/hcc36_temp_upcoded_event_and_time_labels.csv"),
    show_col_types = FALSE
  )

  # remove temporary directory and files
  on.exit(unlink(here("temp/"), recursive = TRUE), add = TRUE)

  # Because HCC35 was coded, no rows of HCC36 should be coded (b/c of hierarchy)
  expect_equal(unique(upcoded_events_hcc36$event_type), 0)
})
