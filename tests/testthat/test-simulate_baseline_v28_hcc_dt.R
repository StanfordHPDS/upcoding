test_that("simulate_baseline_v28_hcc_dt creates output directory if it doesn't exist", {
  test_out_dir <- "test_simulate_output"

  # Run function
  simulate_baseline_v28_hcc_dt(
    num_people = 10,
    out_dir = test_out_dir,
    out_file_prefix = "test_baseline"
  )

  # Check directory was created
  expect_true(dir.exists(here::here(test_out_dir)))

  # remove temporary directory and files
  on.exit(unlink(here::here(test_out_dir), recursive = TRUE), add = TRUE)
})

test_that("simulate_baseline_v28_hcc_dt creates correct output file", {
  test_out_dir <- "test_simulate_output2"

  simulate_baseline_v28_hcc_dt(
    num_people = 50,
    out_dir = test_out_dir,
    out_file_prefix = "test_baseline"
  )

  # Check file exists
  output_file <- here::here(test_out_dir, "test_baseline.csv")
  expect_true(file.exists(output_file))

  # Read and check content
  result_df <- readr::read_csv(output_file, show_col_types = FALSE)

  # remove temporary directory and files
  on.exit(unlink(here::here(test_out_dir), recursive = TRUE), add = TRUE)

  # Check dimensions
  expect_equal(nrow(result_df), 50)
  expect_equal(ncol(result_df), 116) # person_id + 115 HCCs

  # Check column names
  expect_true("person_id" %in% colnames(result_df))
  expect_true(all(v28_hccs %in% colnames(result_df)))

  # Check person_id values
  expect_equal(result_df$person_id, 1:50)

  # Check all values are 0 or 1
  hcc_cols <- result_df |> dplyr::select(-person_id)
  expect_true(all(unlist(hcc_cols) %in% c(0, 1)))
})

test_that("simulate_baseline_v28_hcc_dt respects seed for reproducibility", {
  test_out_dir1 <- "test_seed1"
  test_out_dir2 <- "test_seed2"

  # Run with same seed twice
  simulate_baseline_v28_hcc_dt(
    num_people = 20,
    out_dir = test_out_dir1,
    out_file_prefix = "baseline1",
    curr_seed = 42
  )

  simulate_baseline_v28_hcc_dt(
    num_people = 20,
    out_dir = test_out_dir2,
    out_file_prefix = "baseline2",
    curr_seed = 42
  )

  # Read results
  df1 <- readr::read_csv(
    here::here(test_out_dir1, "baseline1.csv"),
    show_col_types = FALSE
  )
  df2 <- readr::read_csv(
    here::here(test_out_dir2, "baseline2.csv"),
    show_col_types = FALSE
  )

  # remove temporary directory and files
  on.exit(unlink(here::here(test_out_dir1), recursive = TRUE), add = TRUE)
  on.exit(unlink(here::here(test_out_dir2), recursive = TRUE), add = TRUE)

  # Should be identical
  expect_equal(df1, df2)
})

test_that("simulate_baseline_v28_hcc_dt applies hierarchy when requested", {
  test_out_dir_no_hierarchy <- "test_no_hierarchy"
  test_out_dir_with_hierarchy <- "test_with_hierarchy"

  # Create a small reference dataset that will have hierarchical relationships
  # Use HCCs that we know have hierarchy relationships
  test_reference <- data.table::data.table(
    hcc_set = c("hcc17, hcc18, hcc19", "hcc35, hcc36, hcc37, hcc38"),
    num_respondents = c(50, 50)
  )

  # Run without hierarchy
  simulate_baseline_v28_hcc_dt(
    num_people = 10,
    out_dir = test_out_dir_no_hierarchy,
    reference_cooccurring_hcc_count = test_reference,
    apply_v28_hierarchy = FALSE,
    out_file_prefix = "no_hierarchy",
    curr_seed = 123
  )

  # Run with hierarchy
  simulate_baseline_v28_hcc_dt(
    num_people = 10,
    out_dir = test_out_dir_with_hierarchy,
    reference_cooccurring_hcc_count = test_reference,
    apply_v28_hierarchy = TRUE,
    out_file_prefix = "with_hierarchy",
    curr_seed = 123
  )

  # Read results
  df_no_hierarchy <- readr::read_csv(
    here::here(test_out_dir_no_hierarchy, "no_hierarchy.csv"),
    show_col_types = FALSE
  )
  df_with_hierarchy <- readr::read_csv(
    here::here(test_out_dir_with_hierarchy, "with_hierarchy.csv"),
    show_col_types = FALSE
  )

  # remove temporary directory and files
  on.exit(
    unlink(here::here(test_out_dir_no_hierarchy), recursive = TRUE),
    add = TRUE
  )
  on.exit(
    unlink(here::here(test_out_dir_with_hierarchy), recursive = TRUE),
    add = TRUE
  )

  # With hierarchy applied, there should be fewer total HCCs coded
  total_no_hierarchy <- sum(df_no_hierarchy |> dplyr::select(-person_id))
  total_with_hierarchy <- sum(df_with_hierarchy |> dplyr::select(-person_id))

  expect_true(total_with_hierarchy <= total_no_hierarchy)
})

test_that("simulate_baseline_v28_hcc_dt handles edge cases", {
  test_out_dir <- "test_edge_cases"

  # Test with 1 person
  simulate_baseline_v28_hcc_dt(
    num_people = 1,
    out_dir = test_out_dir,
    out_file_prefix = "one_person"
  )

  df_one <- readr::read_csv(
    here::here(test_out_dir, "one_person.csv"),
    show_col_types = FALSE
  )
  expect_equal(nrow(df_one), 1)
  expect_equal(ncol(df_one), 116)

  # Test with large number
  simulate_baseline_v28_hcc_dt(
    num_people = 1000,
    out_dir = test_out_dir,
    out_file_prefix = "many_people"
  )

  df_many <- readr::read_csv(
    here::here(test_out_dir, "many_people.csv"),
    show_col_types = FALSE
  )

  # remove temporary directory and files
  on.exit(unlink(here::here(test_out_dir), recursive = TRUE), add = TRUE)

  expect_equal(nrow(df_many), 1000)
})
