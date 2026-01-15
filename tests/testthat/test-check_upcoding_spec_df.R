test_that("check_upcoding_spec_df_class works", {
  # Example valid spec df
  hcc <- c("hcc152", "hcc107", "hcc139", "hcc226")
  approach <- c("any", "any", "any", "lower severity") # What approach to use to select
  upcoding_prop <- c(0.5, 0.8, 0.75, 0.43) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  expect_no_error(check_upcoding_spec_df_class(my_upcoding_spec_df))

  # invalid spec_df class
  other_specs <- list()
  other_specs[["hcc"]] <- c("hcc152", "hcc107", "hcc139", "hcc226")
  other_specs[["approach"]] <- c("any", "any", "any", "lower severity")
  other_specs[["upcoding_prop"]] <- c(0.5, 0.8, 0.75, 0.43)

  expect_error(check_upcoding_spec_df_class(other_specs))
})

test_that("check_upcoding_spec_df_colnames works", {
  # Example valid spec df
  hcc <- c("hcc152", "hcc107", "hcc139", "hcc226")
  approach <- c("any", "any", "any", "lower severity") # What approach to use to select
  upcoding_prop <- c(0.5, 0.8, 0.75, 0.43) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  expect_no_error(check_upcoding_spec_df_colnames(my_upcoding_spec_df))

  # upper case should still be ok
  colnames(my_upcoding_spec_df) <- toupper(colnames(my_upcoding_spec_df))
  expect_no_error(check_upcoding_spec_df_colnames(my_upcoding_spec_df))

  # try an invalid column name
  colnames(my_upcoding_spec_df)[2] <- "other thing"
  expect_error(check_upcoding_spec_df_colnames(my_upcoding_spec_df))
})


test_that("check_upcoding_spec_df_hccs works", {
  # Example valid spec df
  hcc <- c("hcc152", "hcc107", "hcc139", "hcc226")
  approach <- c("any", "any", "any", "lower severity") # What approach to use to select
  upcoding_prop <- c(0.5, 0.8, 0.75, 0.43) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  expect_no_error(check_upcoding_spec_df_hccs(my_upcoding_spec_df))

  # upper case should still be ok
  my_upcoding_spec_df$hcc <- toupper(my_upcoding_spec_df$hcc)
  expect_no_error(check_upcoding_spec_df_hccs(my_upcoding_spec_df))

  # hcc97 is an invalid v28 HCC
  my_upcoding_spec_df$hcc <- c("hcc97", "hcc383", "hcc78", "hcc17")
  expect_error(check_upcoding_spec_df_hccs(my_upcoding_spec_df))
})

test_that("check_upcoding_spec_df_approach works", {
  # Example valid spec df
  hcc <- c("hcc152", "hcc107", "hcc139", "hcc226")
  approach <- c("any", "any", "any", "lower severity") # What approach to use to select
  upcoding_prop <- c(0.5, 0.8, 0.75, 0.43) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  expect_no_error(check_upcoding_spec_df_approach(my_upcoding_spec_df))

  # upper case should still be fine
  my_upcoding_spec_df$approach <- toupper(my_upcoding_spec_df$approach)
  expect_no_error(check_upcoding_spec_df_approach(my_upcoding_spec_df))

  # causing an error? not sure why
  my_upcoding_spec_df$approach[3] <- "best"
  expect_error(check_upcoding_spec_df_approach(my_upcoding_spec_df))
})

test_that("check_upcoding_spec_df_props works", {
  # Example valid spec df
  hcc <- c("hcc152", "hcc107", "hcc139", "hcc226")
  approach <- c("any", "any", "any", "lower severity") # What approach to use to select
  upcoding_prop <- c(0.5, 0.8, 0.75, 0.43) # Degree of upcoding overall
  my_upcoding_spec_df <- tibble(
    hcc,
    approach,
    upcoding_prop
  )

  expect_no_error(check_upcoding_spec_df_props(my_upcoding_spec_df))

  # one type of invalid prop is 0 or smaller
  my_upcoding_spec_df$upcoding_prop[3] <- 0
  expect_error(check_upcoding_spec_df_props(my_upcoding_spec_df))

  # one type of invalid prop is 0 or smaller
  my_upcoding_spec_df$upcoding_prop[3] <- -1.1
  expect_error(check_upcoding_spec_df_props(my_upcoding_spec_df))

  # one type of invalid prop is 1 or greater
  my_upcoding_spec_df$upcoding_prop[3] <- 1
  expect_error(check_upcoding_spec_df_props(my_upcoding_spec_df))

  # one type of invalid prop is 1 or greater
  my_upcoding_spec_df$upcoding_prop[3] <- 2.2
  expect_error(check_upcoding_spec_df_props(my_upcoding_spec_df))
})
