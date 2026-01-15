test_that("upcode_all_hccs() works as expected", {
  withr::local_seed(123)

  hcc <- c("hcc35", "hcc2")
  approach <- c("any", "any") # What approach to use to select
  upcoding_prop <- c(0.5, 0.5) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble::tibble(
    hcc,
    approach,
    upcoding_prop
  )

  mini_dt <- data.table(matrix(0, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  upcode_all_hccs(
    mini_dt,
    my_upcoding_spec_df,
    "temp"
  )

  # Read in generated data
  hcc2_event_labels <- readr::read_csv(
    here::here("temp/hcc2_upcoded_data_event_and_time_labels.csv"),
    show_col_types = FALSE
  )
  hcc35_event_labels <- readr::read_csv(
    here::here("temp/hcc35_upcoded_data_event_and_time_labels.csv"),
    show_col_types = FALSE
  )
  upcoded_dt <- data.table::fread(here::here("temp/all_hcc_upcoded_data.csv"))

  # remove temporary directory and files
  on.exit(unlink(here("temp/"), recursive = TRUE), add = TRUE)

  # Check count of labeled events for hcc35
  expect_lte(sum(hcc35_event_labels$event_type), 5) # should be 5 if no additional censoring, less otherwise
  expect_equal(sort(hcc35_event_labels$person_id), seq(1:10))

  # Make sure person IDs of upcoded people for hcc35 are equal
  hcc35_label_person_ids <- hcc35_event_labels |>
    dplyr::filter(event_type == 1) |>
    dplyr::pull(person_id)
  hcc35_dt_person_ids <- which(upcoded_dt$hcc35 == 1)
  expect_equal(sort(hcc35_label_person_ids), sort(hcc35_dt_person_ids))

  # Check count of labeled events for hcc2
  expect_lte(sum(hcc2_event_labels$event_type), 5) # should be 5 if no additional censoring, less otherwise
  expect_equal(sort(hcc2_event_labels$person_id), seq(1:10))

  # Make sure person IDs of upcoded people for hcc2 are equal
  hcc2_label_person_ids <- hcc2_event_labels |>
    dplyr::filter(event_type == 1) |>
    dplyr::pull(person_id)
  hcc2_dt_person_ids <- which(upcoded_dt$hcc2 == 1)
  expect_equal(sort(hcc2_label_person_ids), sort(hcc2_dt_person_ids))

  # Check final data.table
  expect_equal(sum(upcoded_dt$hcc2), sum(hcc2_event_labels$event_type))
  expect_equal(sum(upcoded_dt$hcc35), sum(hcc35_event_labels$event_type))
})


test_that("upcode_all_hccs() works as expected with more rows", {
  withr::local_seed(123)

  hcc <- c("hcc35", "hcc2")
  approach <- c("any", "any") # What approach to use to select
  upcoding_prop <- c(0.5, 0.5) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble::tibble(
    hcc,
    approach,
    upcoding_prop
  )

  mini_dt <- data.table(matrix(0, 1000000, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:1000000)

  upcode_all_hccs(
    mini_dt,
    my_upcoding_spec_df,
    "temp"
  )

  # Read in generated data
  hcc2_event_labels <- readr::read_csv(
    here::here("temp/hcc2_upcoded_data_event_and_time_labels.csv"),
    show_col_types = FALSE
  )
  hcc35_event_labels <- readr::read_csv(
    here::here("temp/hcc35_upcoded_data_event_and_time_labels.csv"),
    show_col_types = FALSE
  )
  upcoded_dt <- data.table::fread(here::here("temp/all_hcc_upcoded_data.csv"))

  # remove temporary directory and files
  on.exit(unlink(here("temp/"), recursive = TRUE), add = TRUE)

  # Check count of labeled events for hcc35
  expect_lte(sum(hcc35_event_labels$event_type), 500000) # should be 500,000 if no additional censoring, less otherwise
  expect_equal(sort(hcc35_event_labels$person_id), seq(1:1000000))

  # Make sure person IDs of upcoded people for hcc35 are equal
  hcc35_label_person_ids <- hcc35_event_labels |>
    dplyr::filter(event_type == 1) |>
    dplyr::pull(person_id)
  hcc35_dt_person_ids <- which(upcoded_dt$hcc35 == 1)
  expect_equal(sort(hcc35_label_person_ids), sort(hcc35_dt_person_ids))

  # Check count of labeled events for hcc2
  expect_lte(sum(hcc2_event_labels$event_type), 500000) # should be 5 if no additional censoring, less otherwise
  expect_equal(sort(hcc2_event_labels$person_id), seq(1:1000000))

  # Make sure person IDs of upcoded people for hcc2 are equal
  hcc2_label_person_ids <- hcc2_event_labels |>
    dplyr::filter(event_type == 1) |>
    dplyr::pull(person_id)
  hcc2_dt_person_ids <- which(upcoded_dt$hcc2 == 1)
  expect_equal(sort(hcc2_label_person_ids), sort(hcc2_dt_person_ids))

  # Check final data.table
  expect_equal(sum(upcoded_dt$hcc2), sum(hcc2_event_labels$event_type))
  expect_equal(sum(upcoded_dt$hcc35), sum(hcc35_event_labels$event_type))
})
