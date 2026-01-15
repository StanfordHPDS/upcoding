test_that("make_reference_dataset returns correct structure", {
  # Use minimal test data
  test_data <- tibble::tibble(
    hcc_set = c("hcc1", "hcc2", "hcc1, hcc2"),
    num_respondents = c(100, 150, 50)
  )

  result <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.25
  )

  # Check it's a data.table
  expect_true(data.table::is.data.table(result))

  # Check columns
  expect_equal(colnames(result), c("hcc_set", "num_respondents"))

  # Check data types
  expect_type(result$hcc_set, "character")
  expect_type(result$num_respondents, "double")

  # Check all num_respondents are positive integers
  expect_true(all(result$num_respondents > 0))
  expect_true(all(result$num_respondents == round(result$num_respondents)))

  # Check that original HCCs are still represented in the result
  # (Note: original hcc_sets may be modified with additional HCCs during insertion)
  original_hccs <- test_data$hcc_set |>
    stringr::str_split(", ") |>
    unlist() |>
    unique()

  result_hccs <- result$hcc_set |>
    stringr::str_split(", ") |>
    unlist() |>
    unique()

  expect_true(all(original_hccs %in% result_hccs))
})

test_that("make_reference_dataset handles empty input", {
  empty_data <- tibble::tibble(
    hcc_set = character(0),
    num_respondents = numeric(0)
  )

  # Should handle gracefully - probably return dataset with all missing HCCs
  expect_no_error(
    result <- make_reference_dataset(
      coocurring_hcc_counts = empty_data,
      missing_hcc_insertion_proportion = 0.25
    )
  )

  # Should have at least the missing HCCs
  expect_gte(nrow(result), length(v28_hccs))
})

test_that("make_reference_dataset handles all HCCs present", {
  # Create dataset with all v28 HCCs
  all_hccs_data <- tibble::tibble(
    hcc_set = v28_hccs,
    num_respondents = rep(100, length(v28_hccs))
  )

  result <- make_reference_dataset(
    coocurring_hcc_counts = all_hccs_data,
    missing_hcc_insertion_proportion = 0.25
  )

  # Should not add any new individual HCCs (but might still modify existing ones)
  # Result should have at least as many rows as input
  expect_gte(nrow(result), nrow(all_hccs_data))

  # All v28 HCCs should still be present
  all_hccs_in_result <- result$hcc_set |>
    stringr::str_split(", ") |>
    unlist() |>
    unique()

  expect_true(all(v28_hccs %in% all_hccs_in_result))
})

test_that("make_reference_dataset handles single HCC input", {
  single_hcc_data <- tibble::tibble(
    hcc_set = "hcc1",
    num_respondents = 100
  )

  result <- make_reference_dataset(
    coocurring_hcc_counts = single_hcc_data,
    missing_hcc_insertion_proportion = 0.25
  )

  # Should have many more rows (original + all missing HCCs)
  expect_gt(nrow(result), 100)

  # All v28 HCCs should be present somewhere
  all_hccs_in_result <- result$hcc_set |>
    stringr::str_split(", ") |>
    unlist() |>
    unique()

  expect_true(all(v28_hccs %in% all_hccs_in_result))
})

test_that("make_reference_dataset respects insertion proportion", {
  withr::local_seed(123)
  test_data <- tibble::tibble(
    hcc_set = c("hcc1", "hcc2"),
    num_respondents = c(100, 100)
  )

  # Test with different proportions
  result_low <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.1
  )

  result_high <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.9
  )

  # With higher proportion, more HCC sets should have additional HCCs
  avg_hccs_low <- mean(stringr::str_count(result_low$hcc_set, "hcc"))
  avg_hccs_high <- mean(stringr::str_count(result_high$hcc_set, "hcc"))

  expect_true(avg_hccs_high > avg_hccs_low)
})

test_that("make_reference_dataset validates insertion proportion", {
  test_data <- tibble::tibble(
    hcc_set = c("hcc1", "hcc2"),
    num_respondents = c(100, 100)
  )

  # Test edge cases
  expect_no_error(
    make_reference_dataset(
      coocurring_hcc_counts = test_data,
      missing_hcc_insertion_proportion = 0
    )
  )

  expect_no_error(
    make_reference_dataset(
      coocurring_hcc_counts = test_data,
      missing_hcc_insertion_proportion = 1
    )
  )

  # Test invalid proportions (if validation is added)
  # These might need to be updated based on actual validation behavior
  expect_error(
    make_reference_dataset(
      coocurring_hcc_counts = test_data,
      missing_hcc_insertion_proportion = -0.5
    ),
    "proportion.*between 0 and 1"
  )

  expect_error(
    make_reference_dataset(
      coocurring_hcc_counts = test_data,
      missing_hcc_insertion_proportion = 1.5
    ),
    "proportion.*between 0 and 1"
  )
})

test_that("make_reference_dataset is deterministic with seed", {
  test_data <- tibble::tibble(
    hcc_set = c("hcc1", "hcc2", "hcc3"),
    num_respondents = c(100, 150, 50)
  )

  # Run with same seed twice
  result1 <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.5
  )

  result2 <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.5
  )

  # Should be identical
  expect_equal(result1, result2)

  # Without seed, should differ
  result3 <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.5,
    curr_seed = 999
  )

  result4 <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.5,
    curr_seed = 111
  )

  # Very unlikely to be identical due to randomness
  expect_false(identical(result3, result4))
})

test_that("make_reference_dataset maintains data integrity", {
  test_data <- tibble::tibble(
    hcc_set = c("hcc1", "hcc2", "hcc1, hcc2", "hcc3, hcc4, hcc5"),
    num_respondents = c(100, 150, 50, 200)
  )

  result <- make_reference_dataset(
    coocurring_hcc_counts = test_data,
    missing_hcc_insertion_proportion = 0.3
  )

  # Check no empty hcc_sets
  expect_true(all(nchar(result$hcc_set) > 0))

  # Check HCC format is maintained (all start with "hcc" followed by digits)
  all_hccs <- result$hcc_set |>
    stringr::str_split(", ") |>
    unlist() |>
    unique()

  expect_true(all(stringr::str_detect(all_hccs, "^hcc\\d+$")))

  # Check no duplicate HCCs within a set using functional approach
  has_duplicates <- result$hcc_set |>
    purrr::map(\(.x) stringr::str_split(.x, ", ")[[1]]) |>
    purrr::map_lgl(\(.x) length(.x) != length(unique(.x)))

  expect_false(any(has_duplicates))

  # Check num_respondents are reasonable
  expect_true(all(result$num_respondents > 0))
  expect_true(all(is.finite(result$num_respondents)))
})
