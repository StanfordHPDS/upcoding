test_that("set to zero works for one set of columns", {
  mini_dt <- data.table(matrix(0, 4, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- c(1, 2, 3, 4) # set ids

  # set one reference column and one other value to 1
  mini_dt[c(1, 3), "hcc35" := 1]
  mini_dt[1, "hcc37" := 1]
  mini_dt[4, "hcc36" := 1]
  mini_dt[4, "hcc38" := 1]

  expected_dt <- copy(mini_dt)
  expected_dt[1, "hcc37" := 0]

  set_to_zero(mini_dt, c("hcc36", "hcc37", "hcc38"), "hcc35")

  expect_equal(mini_dt, expected_dt)
})


test_that("apply_v28_cmshcc_hierarchy() works as expected", {
  mini_dt <- data.table(matrix(1, 10, 116))
  colnames(mini_dt) <- c("person_id", v28_hccs)
  mini_dt$person_id <- seq(1:10)

  apply_v28_cmshcc_hierarchy(mini_dt)

  # Only hcc17, hcc35, hcc62, hcc77, hcc80, hcc83, hcc107, hcc111, hcc114, hcc125, hcc135, hcc151
  # hcc180, hcc191, hcc195, hcc211, hcc221, hcc248, hcc253, hcc263, hcc276, hcc282, hcc326, hcc379
  # hcc397, hcc405 and HCCs without hierarchy should remain (51 total)
  coding_sum <- sum(mini_dt[, .SD, .SDcols = !c("person_id")])
  expect_equal(coding_sum, 510)
})
